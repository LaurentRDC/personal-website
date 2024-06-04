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

Getting you the cheapest electricity every second of the year requires **coordination between all operating entities on the power grid**. The modern way to coordinate the behavior of the power grid at all levels are so-called *wholesale electricity markets*. At wholesale electricity markets, power station owners, transmission line owners, and distribution utilities come together to enter auctions to determine the price of electricity. While the details of such auctions are far beyond the scope of this blog post, the important bit is that power auctions answer the following two questions:

1. Where does the energy produced by a power station get consumed?
2. What is the cost of this energy?

Deregulated wholesale electricity markets have been shown to increase competition and lower electricity rates, as well as integrate renewable energy resources more effectively. 

### Local electricity distribution

Powerweave operates at the last power grid level, local distribution.

It used to be that local distribution of electricity was squarely in one direction: homes and businesses would only consume electricity. With the advent of rooftop solar panels and home batteries, **this is no longer true**. Homes and businesses can act as spontaneous, small power stations called *distributed energy resources*.

One one hand, we should encourage individuals and businesses to produce their own energy and even export it back to the grid at certain times. Electricity consumption per capita is increasing rapidly, which is stressing power infrastructure across the world. Individuals and businesses that produce and consume energy, *prosumers*, invest in power infrastructure so that the utilities don't have to -- a concept with the awesome name of *virtual infrastructure upgrade*. The alternative is that electricity distributors invest in infrastructure, in which case *everyone* will split the bill.

On the other hand, electricity distributors are either not ready or unwilling to absorb spontaneous extra energy from prosumers. From an electricity distributor point-of-view, it would be much better if electricity was consumed close to where it is generated (*community self-consumption*).

At the local distribution level, what is now needed is **coordination between all rate-payers in a community**. Does this problem remind you of something?

### So what is Powerweave?

Powerweave operates a platform that coordinates all rate-payers in a community. It solves the problem of incentivizing individuals and businesses to invest in power infrastructure, and others to be more energy efficient, while also ensuring community self-consumption.

The Powerweave platform is centered around local *power auctions*, or *local electricity markets*. For each community, auctions are conducted at regular intervals (usually 5 minutes). Just like wholesale power markets, each auction covers a period of electricity consumption/generation in the near future:

![](/images/powerweave-launch/pw-auction-structure.png)

The time between an auction ending, and its corresponding billing period starting, is the *action period*. This is a period during which individuals and businesses *know* how much power will be, but they still have influence over how much power they consume or generate.

I cannot stress enough how the existence of the action period is key to Powerweave. Consider this example. 

> I am charging my electric car, which will require quite a bit of power (10kW) over the next 8 hours. I participate in the next power auction, where I bid for 10kW of power. However, when closing the auction, I am only cleared to draw 4kW of solar power from a few neighbors at a reasonable price; the shortfall (6kW) will be covered by my utility at a higher price. Knowing this in advance, I can slow down or pause charging my vehicle until I can get a better overall energy price.

This example is the foundation of Powerweave. As a rate-payer, I minimized my electricity bill and used clean sustainable power. My utility incentivized me to be more energy efficient and saved on infrastructure costs. Win-win!

However, as you can imagine, constantly participating in auctions and adjusting power consumption/generation (day and night!) is unrealistic for most people. Therefore, **Powerweave automates all of this**.

On the auction side, Powerweave can trade on behalf of rate-payers. Using statistical and machine-learning models trained on historical consumption and generation data, as well as external data such as weather forecasts, Powerweave can place bids and offers for power to achieve individual goals, such as minimizing energy costs, or minimizing carbon emissions. These models are tireless and self-adjusting; you never need to participate in auctions directly if you don't want to.

On the pricing response side, Powerweave integrates with third-parties (e.g. smartphone notifications, electric vehicle charging systems, smart thermostats) to automatically adjust your load or generation profile.


