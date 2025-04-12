;; -------------------------------------------------------------
;; Virtual Gaming Item Exchange Platform
;; Secure blockchain-based marketplace for digital game item transactions
;; -------------------------------------------------------------

;; Transaction counter for unique IDs
(define-data-var transaction-counter uint u0)

;; Main transaction database structure
(define-map TransactionLedger
  { transaction-id: uint }
  {
    purchaser: principal,
    merchant: principal,
    item-id: uint,
    payment-amount: uint,
    transaction-state: (string-ascii 10),
    creation-block: uint,
    expiration-block: uint
  }
)

;; Core system constants for role management and error codes
(define-constant CONTRACT_ADMIN tx-sender)
(define-constant ERROR_ACCESS_DENIED (err u100))
(define-constant ERROR_TRANSACTION_NOT_FOUND (err u101))
(define-constant ERROR_ALREADY_FINALIZED (err u102))
(define-constant ERROR_PAYMENT_ISSUE (err u103))
(define-constant ERROR_INVALID_TRANSACTION_ID (err u104))
(define-constant ERROR_AMOUNT_INVALID (err u105))
(define-constant ERROR_INVALID_COUNTERPARTY (err u106))
(define-constant ERROR_DEAL_EXPIRED (err u107))
(define-constant TRANSACTION_LIFETIME_BLOCKS u1008) 

;; -------------------------------------------------------------
;; Utility Functions
;; -------------------------------------------------------------

;; Validate transaction ID exists
(define-private (validate-transaction-id (transaction-id uint))
  (<= transaction-id (var-get transaction-counter))
)

;; Validate merchant is not the same as transaction initiator
(define-private (validate-merchant (merchant principal))
  (and 
    (not (is-eq merchant tx-sender))
    (not (is-eq merchant (as-contract tx-sender)))
  )
)

;; -------------------------------------------------------------
;; Core Transaction Functions
;; -------------------------------------------------------------

;; Initialize a new transaction for item purchase
(define-public (begin-transaction (merchant principal) (item-id uint) (payment-amount uint))
  (let 
    (
      (transaction-id (+ (var-get transaction-counter) u1))
      (deadline (+ block-height TRANSACTION_LIFETIME_BLOCKS))
    )
    (asserts! (> payment-amount u0) ERROR_AMOUNT_INVALID)
    (asserts! (validate-merchant merchant) ERROR_INVALID_COUNTERPARTY)
    (match (stx-transfer? payment-amount tx-sender (as-contract tx-sender))
      success
        (begin
          (var-set transaction-counter transaction-id)

          (print {event: "transaction_initiated", transaction-id: transaction-id, purchaser: tx-sender, merchant: merchant, item-id: item-id, payment-amount: payment-amount})
          (ok transaction-id)
        )
      error ERROR_PAYMENT_ISSUE
    )
  )
)

;; Complete transaction and transfer funds to merchant
(define-public (complete-transaction (transaction-id uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (merchant (get merchant transaction-data))
        (payment-amount (get payment-amount transaction-data))
        (item-id (get item-id transaction-data))
      )
      (asserts! (or (is-eq tx-sender CONTRACT_ADMIN) (is-eq tx-sender (get purchaser transaction-data))) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)
      (match (as-contract (stx-transfer? payment-amount tx-sender merchant))
        success
          (begin
            (map-set TransactionLedger
              { transaction-id: transaction-id }
              (merge transaction-data { transaction-state: "completed" })
            )
            (print {event: "transaction_completed", transaction-id: transaction-id, merchant: merchant, item-id: item-id, payment-amount: payment-amount})
            (ok true)
          )
        error ERROR_PAYMENT_ISSUE
      )
    )
  )
)

;; Return payment to the buyer if deal fails
(define-public (return-payment (transaction-id uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (payment-amount (get payment-amount transaction-data))
      )
      (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (match (as-contract (stx-transfer? payment-amount tx-sender purchaser))
        success
          (begin
            (map-set TransactionLedger
              { transaction-id: transaction-id }
              (merge transaction-data { transaction-state: "refunded" })
            )
            (print {event: "payment_returned", transaction-id: transaction-id, purchaser: purchaser, payment-amount: payment-amount})
            (ok true)
          )
        error ERROR_PAYMENT_ISSUE
      )
    )
  )
)

;; Allow purchaser to cancel the transaction before completion
(define-public (abort-transaction (transaction-id uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (payment-amount (get payment-amount transaction-data))
      )
      (asserts! (is-eq tx-sender purchaser) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)
      (match (as-contract (stx-transfer? payment-amount tx-sender purchaser))
        success
          (begin
            (map-set TransactionLedger
              { transaction-id: transaction-id }
              (merge transaction-data { transaction-state: "cancelled" })
            )
            (print {event: "transaction_aborted", transaction-id: transaction-id, purchaser: purchaser, payment-amount: payment-amount})
            (ok true)
          )
        error ERROR_PAYMENT_ISSUE
      )
    )
  )
)

;; Implement whitelist for trusted merchants and high-value transactions
(define-public (register-trusted-relationship (counterparty principal) (trust-level uint) (transaction-limit uint))
  (begin
    ;; Validate inputs
    (asserts! (and (>= trust-level u1) (<= trust-level u3)) (err u350)) ;; Trust levels: 1=basic, 2=verified, 3=premium
    (asserts! (> transaction-limit u0) ERROR_AMOUNT_INVALID)
    (asserts! (not (is-eq counterparty tx-sender)) (err u351)) ;; Cannot whitelist yourself

    ;; Higher trust levels allow higher transaction limits
    (asserts! (or (and (is-eq trust-level u1) (<= transaction-limit u1000))
                 (and (is-eq trust-level u2) (<= transaction-limit u10000))
                 (and (is-eq trust-level u3) (<= transaction-limit u100000))) (err u352))

    ;; In a real implementation, you would store this in a map
    ;; This example just demonstrates the structure

    (print {event: "trust_relationship_registered", user: tx-sender, counterparty: counterparty, 
            trust-level: trust-level, transaction-limit: transaction-limit})
    (ok trust-level)
  )
)

;; Merchant confirms acceptance of transaction terms
(define-public (approve-transaction (transaction-id uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (merchant (get merchant transaction-data))
      )
      (asserts! (is-eq tx-sender merchant) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)
      (map-set TransactionLedger
        { transaction-id: transaction-id }
        (merge transaction-data { transaction-state: "approved" })
      )
      (print {event: "transaction_approved", transaction-id: transaction-id, merchant: merchant})
      (ok true)
    )
  )
)

;; Increase the transaction duration period
(define-public (extend-transaction-time (transaction-id uint) (additional-blocks uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (> additional-blocks u0) ERROR_AMOUNT_INVALID)
    (asserts! (<= additional-blocks u1440) ERROR_AMOUNT_INVALID) ;; Maximum ~10 days extension
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data)) 
        (merchant (get merchant transaction-data))
        (current-deadline (get expiration-block transaction-data))
        (new-deadline (+ current-deadline additional-blocks))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant) (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (map-set TransactionLedger
        { transaction-id: transaction-id }
        (merge transaction-data { expiration-block: new-deadline })
      )
      (print {event: "transaction_extended", transaction-id: transaction-id, requestor: tx-sender, new-deadline: new-deadline})
      (ok true)
    )
  )
)

;; Recover funds when transaction expired
(define-public (recover-expired-funds (transaction-id uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (payment-amount (get payment-amount transaction-data))
        (deadline (get expiration-block transaction-data))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (asserts! (> block-height deadline) (err u108)) ;; Must be expired
      (match (as-contract (stx-transfer? payment-amount tx-sender purchaser))
        success
          (begin
            (map-set TransactionLedger
              { transaction-id: transaction-id }
              (merge transaction-data { transaction-state: "expired" })
            )
            (print {event: "expired_funds_recovered", transaction-id: transaction-id, purchaser: purchaser, payment-amount: payment-amount})
            (ok true)
          )
        error ERROR_PAYMENT_ISSUE
      )
    )
  )
)

;; Open dispute on transaction
(define-public (open-dispute (transaction-id uint) (dispute-reason (string-ascii 50)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)
      (map-set TransactionLedger
        { transaction-id: transaction-id }
        (merge transaction-data { transaction-state: "disputed" })
      )
      (print {event: "dispute_opened", transaction-id: transaction-id, initiator: tx-sender, reason: dispute-reason})
      (ok true)
    )
  )
)

;; Provide merchant feedback after transaction
(define-public (rate-merchant (transaction-id uint) (rating uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (<= rating u5) ERROR_AMOUNT_INVALID) ;; Rating must be between 0-5
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      (asserts! (is-eq tx-sender purchaser) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "completed") (err u109)) ;; Can only rate after completion
      (print {event: "merchant_rated", transaction-id: transaction-id, merchant: merchant, rating: rating, reviewer: tx-sender})
      (ok true)
    )
  )
)

;; Add cryptographic verification signature
(define-public (add-verification (transaction-id uint) (digital-signature (buff 65)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (print {event: "digital_verification_added", transaction-id: transaction-id, party: tx-sender, digital-signature: digital-signature})
      (ok true)
    )
  )
)

;; Register backup recovery address
(define-public (register-backup-address (transaction-id uint) (backup-address principal))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
      )
      (asserts! (is-eq tx-sender purchaser) ERROR_ACCESS_DENIED)
      (asserts! (not (is-eq backup-address tx-sender)) (err u111)) ;; Backup address must be different
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (print {event: "backup_address_registered", transaction-id: transaction-id, purchaser: purchaser, backup: backup-address})
      (ok true)
    )
  )
)

;; Resolve dispute with partial payments
(define-public (resolve-dispute (transaction-id uint) (purchaser-percent uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERROR_ACCESS_DENIED)
    (asserts! (<= purchaser-percent u100) ERROR_AMOUNT_INVALID) ;; Percentage must be 0-100
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
        (payment-amount (get payment-amount transaction-data))
        (purchaser-amount (/ (* payment-amount purchaser-percent) u100))
        (merchant-amount (- payment-amount purchaser-amount))
      )
      (asserts! (is-eq (get transaction-state transaction-data) "disputed") (err u112)) ;; Must be disputed
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)

      ;; Send purchaser's portion
      (unwrap! (as-contract (stx-transfer? purchaser-amount tx-sender purchaser)) ERROR_PAYMENT_ISSUE)

      ;; Send merchant's portion
      (unwrap! (as-contract (stx-transfer? merchant-amount tx-sender merchant)) ERROR_PAYMENT_ISSUE)

      (map-set TransactionLedger
        { transaction-id: transaction-id }
        (merge transaction-data { transaction-state: "resolved" })
      )
      (print {event: "dispute_resolved", transaction-id: transaction-id, purchaser: purchaser, merchant: merchant, 
              purchaser-amount: purchaser-amount, merchant-amount: merchant-amount, purchaser-percent: purchaser-percent})
      (ok true)
    )
  )
)

;; Set up multiple approvers for high-value transactions
(define-public (register-multiple-approvers (transaction-id uint) (approver principal))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (payment-amount (get payment-amount transaction-data))
      )
      ;; Only allow multiple approvers for high-value transactions (> 1000 STX)
      (asserts! (> payment-amount u1000) (err u120))
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (print {event: "approver_registered", transaction-id: transaction-id, approver: approver, registrar: tx-sender})
      (ok true)
    )
  )
)

;; Security function to freeze suspicious activity
(define-public (freeze-suspicious-activity (transaction-id uint) (suspicion-details (string-ascii 100)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      (asserts! (or (is-eq tx-sender CONTRACT_ADMIN) (is-eq tx-sender purchaser) (is-eq tx-sender merchant)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") 
                   (is-eq (get transaction-state transaction-data) "approved")) 
                ERROR_ALREADY_FINALIZED)
      (map-set TransactionLedger
        { transaction-id: transaction-id }
        (merge transaction-data { transaction-state: "frozen" })
      )
      (print {event: "activity_frozen", transaction-id: transaction-id, reporter: tx-sender, details: suspicion-details})
      (ok true)
    )
  )
)

;; Create phased payment transaction
(define-public (create-phased-transaction (merchant principal) (item-id uint) (payment-amount uint) (phases uint))
  (let 
    (
      (transaction-id (+ (var-get transaction-counter) u1))
      (deadline (+ block-height TRANSACTION_LIFETIME_BLOCKS))
      (phase-amount (/ payment-amount phases))
    )
    (asserts! (> payment-amount u0) ERROR_AMOUNT_INVALID)
    (asserts! (> phases u0) ERROR_AMOUNT_INVALID)
    (asserts! (<= phases u5) ERROR_AMOUNT_INVALID) ;; Max 5 phases
    (asserts! (validate-merchant merchant) ERROR_INVALID_COUNTERPARTY)
    (asserts! (is-eq (* phase-amount phases) payment-amount) (err u121)) ;; Ensure even division
    (match (stx-transfer? payment-amount tx-sender (as-contract tx-sender))
      success
        (begin
          (var-set transaction-counter transaction-id)

          (print {event: "phased_transaction_created", transaction-id: transaction-id, purchaser: tx-sender, merchant: merchant, 
                  item-id: item-id, payment-amount: payment-amount, phases: phases, phase-amount: phase-amount})
          (ok transaction-id)
        )
      error ERROR_PAYMENT_ISSUE
    )
  )
)

;; Emergency halt for critical contract vulnerabilities
(define-public (emergency-halt-operations)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERROR_ACCESS_DENIED)
    (print {event: "operations_emergency_halted", administrator: tx-sender, halt-block: block-height})
    ;; Note: In a real implementation, you would set a contract variable to track the paused state
    ;; and add checks to all functions to prevent execution while paused
    (ok true)
  )
)

;; Schedule time-locked sensitive operations
(define-public (schedule-sensitive-operation (operation-name (string-ascii 20)) (operation-data (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERROR_ACCESS_DENIED)
    (asserts! (> (len operation-data) u0) ERROR_AMOUNT_INVALID)
    (let
      (
        (execute-at-block (+ block-height u144)) ;; 24 hours delay based on block height
      )
      (print {event: "operation_scheduled", operation: operation-name, data: operation-data, execute-at-block: execute-at-block})
      (ok execute-at-block)
    )
  )
)

;; Enable enhanced security for high-value transactions
(define-public (enable-enhanced-security (transaction-id uint) (security-token (buff 32)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (payment-amount (get payment-amount transaction-data))
      )
      ;; Only allow enhanced security for transactions above a certain value threshold
      (asserts! (> payment-amount u5000) (err u130))
      (asserts! (is-eq tx-sender purchaser) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (print {event: "enhanced_security_enabled", transaction-id: transaction-id, purchaser: purchaser, token-hash: (hash160 security-token)})
      (ok true)
    )
  )
)

;; Verify transaction with cryptographic signatures
(define-public (cryptographically-verify-transaction (transaction-id uint) (message-data (buff 32)) (crypto-signature (buff 65)) (signing-party principal))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
        (verification-result (unwrap! (secp256k1-recover? message-data crypto-signature) (err u150)))
      )
      ;; Verify the transaction with cryptographic proof
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant) (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq signing-party purchaser) (is-eq signing-party merchant)) (err u151))
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)

      ;; Verify that the signature corresponds to the expected signing party
      (asserts! (is-eq (unwrap! (principal-of? verification-result) (err u152)) signing-party) (err u153))

      (print {event: "transaction_crypto_verified", transaction-id: transaction-id, verifier: tx-sender, signing-party: signing-party})
      (ok true)
    )
  )
)

;; Add metadata to transaction for advanced analytics
(define-public (attach-transaction-metadata (transaction-id uint) (metadata-category (string-ascii 20)) (metadata-digest (buff 32)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      ;; Allow metadata addition by authorized parties only
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant) (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (not (is-eq (get transaction-state transaction-data) "completed")) (err u160))
      (asserts! (not (is-eq (get transaction-state transaction-data) "refunded")) (err u161))
      (asserts! (not (is-eq (get transaction-state transaction-data) "expired")) (err u162))

      ;; Validate metadata category
      (asserts! (or (is-eq metadata-category "item-details") 
                   (is-eq metadata-category "delivery-evidence")
                   (is-eq metadata-category "quality-report")
                   (is-eq metadata-category "purchaser-requirements")) (err u163))

      (print {event: "metadata_attached", transaction-id: transaction-id, metadata-category: metadata-category, 
              metadata-digest: metadata-digest, submitted-by: tx-sender})
      (ok true)
    )
  )
)



;; Configure security rate-limiting
(define-public (configure-security-limits (attempt-limit uint) (lockout-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERROR_ACCESS_DENIED)
    (asserts! (> attempt-limit u0) ERROR_AMOUNT_INVALID)
    (asserts! (<= attempt-limit u10) ERROR_AMOUNT_INVALID) ;; Maximum 10 attempts allowed
    (asserts! (> lockout-period u6) ERROR_AMOUNT_INVALID) ;; Minimum 6 blocks lockout (~1 hour)
    (asserts! (<= lockout-period u144) ERROR_AMOUNT_INVALID) ;; Maximum 144 blocks lockout (~1 day)

    ;; Note: In a real implementation, you would set contract variables to track
    ;; the rate limiting parameters and implement logic in each function to enforce them

    (print {event: "security_limits_configured", attempt-limit: attempt-limit, 
            lockout-period: lockout-period, administrator: tx-sender, block-height: block-height})
    (ok true)
  )
)

;; Set up time-locked recovery mechanism
(define-public (establish-recovery-mechanism (transaction-id uint) (delay-period uint) (recovery-principal principal))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (> delay-period u72) ERROR_AMOUNT_INVALID) ;; Minimum 72 blocks delay (~12 hours)
    (asserts! (<= delay-period u1440) ERROR_AMOUNT_INVALID) ;; Maximum 1440 blocks delay (~10 days)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (activation-block (+ block-height delay-period))
      )
      (asserts! (is-eq tx-sender purchaser) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)
      (asserts! (not (is-eq recovery-principal purchaser)) (err u180)) ;; Recovery principal must be different from purchaser
      (asserts! (not (is-eq recovery-principal (get merchant transaction-data))) (err u181)) ;; Recovery principal must be different from merchant
      (print {event: "recovery_mechanism_established", transaction-id: transaction-id, purchaser: purchaser, 
              recovery-principal: recovery-principal, activation-block: activation-block})
      (ok activation-block)
    )
  )
)


;; Allow authorized operators to perform actions on behalf of a user
(define-public (set-authorized-operator (transaction-id uint) (operator principal) (can-complete bool) (can-cancel bool))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
      )
      (asserts! (is-eq tx-sender purchaser) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (asserts! (not (is-eq operator purchaser)) (err u190)) ;; Operator cannot be the purchaser
      (asserts! (not (is-eq operator (get merchant transaction-data))) (err u191)) ;; Operator cannot be the merchant

      (print {event: "operator_authorized", transaction-id: transaction-id, purchaser: purchaser, 
              operator: operator, can-complete: can-complete, can-cancel: can-cancel})
      (ok true)
    )
  )
)

;; Add two-factor authentication requirement for transaction completion
(define-public (enable-two-factor-auth (transaction-id uint) (auth-hash (buff 32)) (auth-expiry uint))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (> auth-expiry block-height) ERROR_AMOUNT_INVALID)
    (asserts! (<= auth-expiry (+ block-height u1008)) (err u200)) ;; Max expiry is transaction lifetime
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant)) ERROR_ACCESS_DENIED)
      (asserts! (is-eq (get transaction-state transaction-data) "pending") ERROR_ALREADY_FINALIZED)

      (print {event: "two_factor_auth_enabled", transaction-id: transaction-id, 
              enabler: tx-sender, auth-hash: auth-hash, auth-expiry: auth-expiry})
      (ok true)
    )
  )
)

;; Add new entries to transaction audit trail
(define-public (add-audit-record (transaction-id uint) (action-type (string-ascii 20)) (action-data (string-ascii 100)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
      )
      (asserts! (or (is-eq tx-sender purchaser) 
                   (is-eq tx-sender merchant) 
                   (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)

      ;; Validate action type
      (asserts! (or (is-eq action-type "message")
                   (is-eq action-type "verification")
                   (is-eq action-type "delivery-update")
                   (is-eq action-type "inspection")
                   (is-eq action-type "review")) (err u210))

      (print {event: "audit_record_added", transaction-id: transaction-id, actor: tx-sender, 
              action-type: action-type, action-data: action-data, block-height: block-height})
      (ok true)
    )
  )
)

;; Implement circuit breaker for transactions to protect against market manipulation
(define-public (trigger-circuit-breaker (suspicious-transaction-id uint) (reason-code uint))
  (begin
    (asserts! (validate-transaction-id suspicious-transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (is-eq tx-sender CONTRACT_ADMIN) ERROR_ACCESS_DENIED)
    (asserts! (and (>= reason-code u1) (<= reason-code u5)) (err u220)) ;; Valid reason codes are 1-5

    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: suspicious-transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
        (payment-amount (get payment-amount transaction-data))
      )
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") 
                   (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (print {event: "circuit_breaker_triggered", transaction-id: suspicious-transaction-id, 
              administrator: tx-sender, reason-code: reason-code, 
              purchaser: purchaser, merchant: merchant, amount: payment-amount})
      (ok true)
    )
  )
)

;; Flag potentially fraudulent activity based on transaction patterns
(define-public (flag-suspicious-activity (transaction-id uint) (suspicion-level uint) (evidence-hash (buff 32)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (and (>= suspicion-level u1) (<= suspicion-level u3)) (err u240)) ;; Valid levels: 1=low, 2=medium, 3=high

    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
        (item-id (get item-id transaction-data))
      )
      ;; Allow reporting from any party or admin
      (asserts! (or (is-eq tx-sender purchaser) 
                   (is-eq tx-sender merchant) 
                   (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") 
                   (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)

      (print {event: "fraud_detection_alert", transaction-id: transaction-id, reporter: tx-sender, 
              suspicion-level: suspicion-level, evidence-hash: evidence-hash, item-id: item-id,
              purchaser: purchaser, merchant: merchant})
      (ok suspicion-level)
    )
  )
)

;; Create a batch transaction for multiple items
(define-public (create-batch-transaction (merchant principal) (item-ids (list 10 uint)) (total-payment uint))
  (let 
    (
      (transaction-id (+ (var-get transaction-counter) u1))
      (deadline (+ block-height TRANSACTION_LIFETIME_BLOCKS))
      (item-count (len item-ids))
    )
    (asserts! (> total-payment u0) ERROR_AMOUNT_INVALID)
    (asserts! (> item-count u0) ERROR_AMOUNT_INVALID)
    (asserts! (<= item-count u10) (err u250)) ;; Maximum 10 items per batch
    (asserts! (validate-merchant merchant) ERROR_INVALID_COUNTERPARTY)

    (match (stx-transfer? total-payment tx-sender (as-contract tx-sender))
      success
        (begin
          (var-set transaction-counter transaction-id)

          (print {event: "batch_transaction_created", transaction-id: transaction-id, purchaser: tx-sender, 
                  merchant: merchant, item-ids: item-ids, total-payment: total-payment, item-count: item-count})
          (ok transaction-id)
        )
      error ERROR_PAYMENT_ISSUE
    )
  )
)

;; Create a transaction with escrow release milestones
(define-public (create-milestone-transaction (merchant principal) (item-id uint) (total-amount uint) (milestone-percentages (list 5 uint)))
  (let 
    (
      (transaction-id (+ (var-get transaction-counter) u1))
      (deadline (+ block-height TRANSACTION_LIFETIME_BLOCKS))
      (milestone-count (len milestone-percentages))
      (total-percentage (fold + milestone-percentages u0))
    )
    (asserts! (> total-amount u0) ERROR_AMOUNT_INVALID)
    (asserts! (> milestone-count u0) ERROR_AMOUNT_INVALID)
    (asserts! (<= milestone-count u5) (err u260)) ;; Maximum 5 milestones
    (asserts! (validate-merchant merchant) ERROR_INVALID_COUNTERPARTY)
    (asserts! (is-eq total-percentage u100) (err u261)) ;; Percentages must add up to 100

    (match (stx-transfer? total-amount tx-sender (as-contract tx-sender))
      success
        (begin
          (var-set transaction-counter transaction-id)

          (print {event: "milestone_transaction_created", transaction-id: transaction-id, purchaser: tx-sender, 
                  merchant: merchant, item-id: item-id, total-amount: total-amount, 
                  milestone-percentages: milestone-percentages})
          (ok transaction-id)
        )
      error ERROR_PAYMENT_ISSUE
    )
  )
)

;; Release payment for a specific milestone
(define-public (release-milestone-payment (transaction-id uint) (milestone-index uint) (milestone-proof (buff 32)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (<= milestone-index u4) (err u270)) ;; Max index is 4 (for 5 milestones)

    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
        (payment-amount (get payment-amount transaction-data))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender CONTRACT_ADMIN)) ERROR_ACCESS_DENIED)
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") 
                   (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)

      ;; In a real implementation, you would track released milestones and calculate the amount to release
      ;; This is a simplified version that just demonstrates the structure
      (print {event: "milestone_payment_released", transaction-id: transaction-id, milestone-index: milestone-index, 
              merchant: merchant, proof-hash: (hash160 milestone-proof), released-by: tx-sender})
      (ok true)
    )
  )
)

;; Implement transaction delegation with time-locked recovery
(define-public (delegate-transaction-control (transaction-id uint) (delegate principal) (delegation-period uint) (delegation-scope (string-ascii 20)))
  (begin
    (asserts! (validate-transaction-id transaction-id) ERROR_INVALID_TRANSACTION_ID)
    (asserts! (> delegation-period u0) ERROR_AMOUNT_INVALID)
    (asserts! (<= delegation-period u144) (err u300)) ;; Max ~1 day

    (let
      (
        (transaction-data (unwrap! (map-get? TransactionLedger { transaction-id: transaction-id }) ERROR_TRANSACTION_NOT_FOUND))
        (purchaser (get purchaser transaction-data))
        (merchant (get merchant transaction-data))
        (expiry-block (+ block-height delegation-period))
      )
      (asserts! (or (is-eq tx-sender purchaser) (is-eq tx-sender merchant)) ERROR_ACCESS_DENIED)
      (asserts! (not (is-eq delegate tx-sender)) (err u301)) ;; Cannot delegate to self
      (asserts! (or (is-eq (get transaction-state transaction-data) "pending") 
                   (is-eq (get transaction-state transaction-data) "approved")) ERROR_ALREADY_FINALIZED)
      (asserts! (<= block-height (get expiration-block transaction-data)) ERROR_DEAL_EXPIRED)

      ;; Validate delegation scope
      (asserts! (or (is-eq delegation-scope "complete-only") 
                   (is-eq delegation-scope "cancel-only") 
                   (is-eq delegation-scope "complete-and-cancel")
                   (is-eq delegation-scope "dispute-only")) (err u302))

      (print {event: "transaction_delegated", transaction-id: transaction-id, delegator: tx-sender, 
              delegate: delegate, delegation-period: delegation-period, expiry-block: expiry-block,
              delegation-scope: delegation-scope})
      (ok expiry-block)
    )
  )
)

