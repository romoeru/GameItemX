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
