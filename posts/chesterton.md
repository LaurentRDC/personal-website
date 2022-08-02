---
title: Chesterton's fence and why I'm not sold on the blockchain
date: 2022-08-02
summary: Chesterton's fence is a principle which says that we should understand why things are the way, before we try to push for change. Cryptocurrencies based on the blockchain haven't demonstrated that they understand the reason for centralization of the current payment networks.
---

The key technological advances which brought Bitcoin to life are the blockchain and its associated proof-of-work consensus algorithm. The Bitcoin whitepaper[^bitcoin-whitepaper] is very clear on its purpose:

> A purely peer-to-peer version of electronic cash would allow online payments to be sent directly from one party to another without going through a financial institution. Digital signatures provide part of the solution, but the main benefits are lost if a trusted third party is still required to prevent double-spending.

The *double-spending* problem to which Nakamoto refers is a unique challenge of digital cash implementations. Contrary to physical cash, which is difficult to copy, digital cash is but bytes; it can be trivially copied. Before Bitcoin, the most popular way to prevent double-spending has been to route all digital cash transactions on a particular network through a trusted entity which ensures that no double-spending occurs. This is how the credit card and Interac networks work, for example. 

The Bitcoin whitepaper brings a new solution to the double-spending problem, a solution designed to explicitly avoid centralized trusted entities.

---

In software engineering, there is a principle that one should understand *why* something is the way it is, before trying to change it. This principle is known as *Chesterton's fence*[^chesterton-fence]:

> There exists (...) a fence or gate erected across a road. The more modern type of reformer goes gaily up to it and says, 'I don't see the use of this; let us clear it away.' To which the more intelligent type of reformer will do well to answer: 'If you don't see the use of it, I certainly won't let you clear it away. Go away and think. Then, when you can come back and tell me that you do see the use of it, I may allow you to destroy it.

To me, the push towards decentralization is a case of Chesterton's fence. No one wants to involve a third-party in every transactions, but it is this way for two main reasons: fraud management and performance (transaction throughput).

**Fraud management** is a weak point of an anonymous peer-to-peer network like Bitcoin. While I appreciate the desire for anonymity, this leads to the same behaviors which lead to the founding of the US Securities and Exchange Commission almost a hundred years ago. Decentralization also enabled the rise of ransomware, as it is now much harder to track the flow of money between anonymous, single-use cryptocurrency accounts.

**Performance** is another major downside of decentralization. As an example, Bitcoin's throughput has never reached more than [6 transactions per second](https://blockchair.com/bitcoin/charts/transactions-per-second) as of the time of writing. By contract, the electronic payment network VisaNet (which powers Visa credit card) can process up to [76 000 transactions per second](https://usa.visa.com/about-visa/visanet.html).

Until blockchain enthusiasts understand the advantages of centralization presented above, I don't think cryptocurrencies will become mainstream.

*This post was inspired by the [Tim O'Reilly interview on the Rational Reminder podcast](https://rationalreminder.ca/podcast/crypto8)*.

[^bitcoin-whitepaper]: S. Nakamoto, *Bitcoin: A Peer-to-Peer Electronic Cash System* (2008). [Link to PDF](https://bitcoin.org/bitcoin.pdf).

[^chesterton-fence]: G. K. Chesterton, *The Thing: Why I Am a Catholic*, chapter 4 (1929).
