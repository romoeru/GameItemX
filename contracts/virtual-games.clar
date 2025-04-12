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
