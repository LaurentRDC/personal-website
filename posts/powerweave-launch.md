---
title: Starting a new adventure with Powerweave
date: 2024-06-01
summary: I started a business, Powerweave. In this post, I explain what Powerweave does and why it is important to me.
---

I have been working nights and weekends on starting a business for the better part of a year. Recently, I told my coworkers that I was pursuing something new. I no longer have to keep it quiet.

Starting a business is something that I have been thinking about since finishing graduate school. [My academic expertise](/about.html) could not be turned into a business outright, but my skills in mathematics, software engineering, and computer science have a broader applicability.

I joined SocïVolta, a trading firm that operates like a technology startup, precisely because I wanted experience from inside a startup. Joining SocïVolta was about more than generic experience, however: I wanted exposure to power grid technologies. 

In April 2024, my co-founder Mathilde Mounier and I founded [Powerweave](https://powerweave.io), a technology startup that operates local electricity markets on behalf of electricity distributors, such as utilities and independent microgrids. 

### What is the North American power grid?

The "power grid" is an electrical network which can be broken down in three broad levels:

* Generation: power stations, which are often far from population centers;
* Transmission: long-distance electrical transmission lines connecting power stations to population centers;
* Distribution: network of low-voltage transmission to homes and businesses.

There are places in North America where all three levels are controlled by a single entity (e.g. Hydro-Québec). But increasingly, different entities own and operate different levels of the power grid.

What is the provenance of the electricity that powers your screen right now? You might think that it comes from the closest power station. In practice, however, the provenance of your electricity depends on lots of factors, including who else wants electricity, and who is currently generating electricity. 

Getting you the cheapest electricity every second of the year requires **coordination between all operating entities on the power grid**. The modern way to coordinate the behavior of the power grid at all levels are so-called *wholesale electricity markets*. At wholesale electricity markets, power station owners, transmission line owners, and distribution utilities come together to enter auctions to determine the price of electricity. While the details of such auctions are far beyond the scope of this blog post[^opf], the important bit is that power auctions answer the following two questions:

1. Where does the energy produced by a power station get consumed?
2. What is the cost of this energy?

[^opf]: Ultimately, the bids and offers for energy, as well as transmission and other constraints, are taken into account to solve an optimization problem known as *optimal power flow*.  

### Powerweave and local electricity markets
