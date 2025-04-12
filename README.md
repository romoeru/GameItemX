# GameItemX

**GameItemX** is a blockchain-based Clarity smart contract system designed to power secure, trustless, and time-sensitive transactions for virtual gaming items. This protocol ensures a safe marketplace environment where players and merchants can interact with confidence.

---

## 🚀 Features

- 🔐 **Secure Escrow-Based Payments**  
  Funds are held securely until terms are fulfilled.

- 📦 **Multi-Phase Transaction Lifecycle**  
  Includes `begin`, `approve`, `complete`, `abort`, and `expire`.

- ⏱️ **Block-Height Based Expiration**  
  All transactions are time-limited using Stacks' `block-height`.

- 📜 **Dispute Resolution**  
  Parties can open disputes to freeze and evaluate transactions.

- ✅ **Merchant Trust Levels**  
  Trust tiers and limits ensure scalable control for larger trades.

- ⭐ **Reputation Feedback**  
  Buyers can rate merchants post-transaction.

- 🔑 **Digital Verification Support**  
  Optional cryptographic signature logging for added authenticity.

---

## 📂 Structure

| Function                     | Description                                     |
|-----------------------------|-------------------------------------------------|
| `begin-transaction`         | Initiate a purchase with locked funds.          |
| `approve-transaction`       | Merchant accepts the deal.                      |
| `complete-transaction`      | Finalize the deal and release payment.          |
| `abort-transaction`         | Buyer cancels the deal before approval.         |
| `return-payment`            | Admin cancels and returns funds.                |
| `recover-expired-funds`     | Buyer retrieves funds from expired deals.       |
| `extend-transaction-time`   | Extend active transaction window.               |
| `open-dispute`              | Raise an issue over the transaction.            |
| `rate-merchant`             | Leave a rating after a completed trade.         |
| `add-verification`          | Attach a digital signature to the transaction.  |

---

## 🛠 Installation

This project is built using [Clarity](https://docs.stacks.co/write-smart-contracts/clarity-overview) for the Stacks blockchain.

To test locally:

1. Install [Clarinet](https://docs.stacks.co/clarity/clarinet/overview)
2. Clone this repo:
   ```bash
   git clone https://github.com/your-username/GameItemX.git
   cd GameItemX
   ```
3. Run tests:
   ```bash
   clarinet test
   ```

---

## 📜 License

MIT License

---


## 🌐 Acknowledgements

- Powered by the [Stacks Blockchain](https://www.stacks.co/)
- Built with [Clarity Smart Contracts](https://docs.stacks.co/)
