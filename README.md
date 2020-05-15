# 3tier
A prototype 3-tier web application written in PureScript.

## Description
* The overarching goal of this project is to develop a model SIEM/IDS solution written in PureScript.
* It is intended to be implemented and deployable as a 3-tier web application, with the goal of exploring the use of PureScript and formal methods to provably mitigate OWASP Top 10 web application security risks within the application.
* The presentation-tier is intended to faciliate incident management & response for incidents triggered by detecting anomalous behaviours of entities on a network, derived from forwarded & audited events.
* The application-tier is intended to faciliate centralized logging of traffic/gateway events, Linux Auditing System events, and Windows Security event, and e.g. vulnerability scan results.
* The data-access tier is intended to faciliate analytics & reporting on forwarded & audited events, as well as:
e.g. security risk management, disaster recovery, and business continuity.
* The project was motivated by the idea of developing e.g. a model & lightweight FOSS alternative to Splunk.

## Status
The project is currently a work-in-progress.
