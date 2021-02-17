# 3tier
A prototype 3-tier web application written in PureScript. Currently extracting re-usable parsing/validation, FFI, control flow, DSL, and event collection packages for a follow-up / future project(s).

**Current Status: (Archived)**
 
*In a follow-up project with analogous overarching goals: I would like to develop a model SIEM/IDS software solution (as a 3-tier web application) written in PureScript, e.g. like a model / lightweight FOSS alternative to Splunk. I would like to continue to explore and evaluate of PureScript and formal methods to manage and "provably" mitigate OWASP Top 10 web application security risks as well. In the near future, I would like to finish: transitioning the current iteration of this project for future projects, conduct a final post-mortem analysis, and review / revise the lessons learned throughout the duration of this project for an analogous future project.* 

## Contents
- [Proposal](##proposal)
  * [Purpose](##purpose)
  * [Introduction](##introduction)
    * [Diagram of 3-Tier Architecture](##diagram) 
  * [Goals](##goals)
    * [Tier 3](##tier-3)
    * [Tier 2](##tier-2)
    * [Tier 1](##tier-1)
  * [Schedule](##schedule)
  * [Issues](##issues)
  * [Timeline](##timeline)

## Proposal

##### Purpose

<p align="justify">
Fileless Malware and Insider Threat incidents are currently causing significant disruptions to the services and operations of industries employing IT. For example, Fileless Malware attacks are currently costing industries, such as healthcare and finance, millions upon millions of dollars due to successful Fileless Malware attacks (e.g. SAMSAM and GoLacker). According to reputable sources, we are currently seeing a dramatic rate of increase in Fileless Malware incidents reported across industries employing IT (<a href="https://www.trendmicro.com/vinfo/us/security/news/security-technology/risks-under-the-radar-understanding-fileless-threats">TrendMicro,2019</a>), and it is possible that the majority of successful cyberattacks now involve Fileless Malware incidents that are currently occurring undetected by targeted organizations (<a href="https://www.carbonblack.com/resources/definitions/what-is-fileless-malware/">Carbon Black, 2019</a>). Our motivational goal for this project is to research & develop a model SIEM/IDS software solution (as a 3-tier web application), that can applied to faciliate security risk management for these types of threats. We are currently seeking contributors to assist in the development, testing, and project management for the current prototype of our 3-tier application. 
</p>

#### Introduction

<p align="justify">
Our current overarching goal for this project is to develop a model SIEM/IDS solution written in PureScript.
It is intended to be implemented and deployable as a 3-tier web application, with the goal of exploring the use of PureScript and formal methods to provably mitigate OWASP Top 10 web application security risks within the application.
The <b>presentation-tier</b> is intended to faciliate incident management & response for incidents triggered by detecting anomalous behaviours of entities on a network, derived from forwarded & audited events.
The <b>application-tier</b> is intended to faciliate centralized logging of traffic/gateway events, Linux Auditing System events, and Windows Security event, and e.g. vulnerability scan results.
The <b>data-access tier</b> is intended to faciliate analytics & reporting on forwarded & audited events, as well as:
e.g. security risk management, disaster recovery, and business continuity.
The current prototype development & testing for our 3-tier application  was modeled / inspired by the idea of developing e.g. a model & lightweight FOSS alternative to Splunk, which we summarize with the following diagram of our proposed 3-tier architecture.
</p>

##### Diagram

![Diagram of 3-Tier Architecture](./README/diagram.svg)

#### Goals

##### Tier 3

*	Awaits *Tier 3* resource requests to retrieve statistics reports about forwarded & audited events.
*	Awaits *Tier 3* resource requests to store forwarded and audited events, abstracting over a suitable choice of DBMS backend(s).
*	Audits all incoming *Tier 3* resource requests according to specification.
*	Enforces authentication, authorization/access control, and project risk management policies for *Tier 3* resource requests.

##### Tier 2

*	Emits *Tier 3* resource requests to retrieve statistics reports about forwarded & audited events.
*	Awaits *Tier 2* resource requests to retrieve statistics reports at the report route(s) defined by the backend web application.
*	Awaits *Tier 2* resource requests to forward Windows Security Event Log and Linux Auditing System data in JSON format at the forward route(s) defined by the backend web application.
*	Awaits *Tier 2* traffic/gateway event data in JSON format, e.g. flow records in SiLk *rwfilter/rwcut* format at the appropriate forward route defined by the backend web application.
*	Audits all incoming *Tier 2* resource requests according to specification.
*	Emits *Tier 3* resource requests to store audited & forwarded events after parsing & validation.
*	Enforces authentication, authorization/access control, and project risk management policies for *Tier 2* resource requests.

##### Tier 1

*	Emits *Tier 2* resource requests to retrieve statistics reports about forwarded & audited events.
* Awaits *Tier 1* resource requests to retrieve summary reports about forwarded & audited events (e.g. in feature matrix format). 
*	Triggers alerts, e.g. based on application-defined rules and/or automated document classification approaches, from sets of *Tier 2* statistics reports.
*	Awaits *Tier 1* resource requests to poll and view triggered alerts at the report route(s) defined by the frontend web application.
*	Awaits *Tier 1* resource requests to push and forward triggered alerts to an incident response platform, such as PagerDuty or JIRA, at the forward route(s) defined by frontend web application.
*	Audits all incoming *Tier 1* resource requests according to specification.
*	Enforces authentication, authorization/access control, and project risk management policies for *Tier 1* resource requests.

#### Schedule

See [milestones](https://github.com/markfarrell/3tier/milestones).

#### Issues

See [issues](https://github.com/markfarrell/3tier/issues).

#### Timeline

See [timeline](./README/timeline.png).
