---
title: Threat-Model Reference Table
description: Exercises multi-column layout with wide Examples column.
---

| Element          | Symbol        | Description                          | Examples                                            |
|------------------|---------------|--------------------------------------|-----------------------------------------------------|
| External Entity  | Rectangle     | Things outside your control          | Users, external systems, web sites                  |
| Process          | Circle        | Running code                         | Services, applications, functions                   |
| Data Flow        | Arrow         | Information movement                 | API calls, network traffic                          |
| Data Store       | Parallel lines| Data at rest                         | Databases, files, caches                            |
| Trust Boundary   | Dotted line   | Change in privilege/trust level      | Network boundaries, process isolation               |
| Security Control | Shield icon   | Mitigation applied to a component    | TLS termination, auth gateway, WAF                  |
| Monitoring       | Eye icon      | Visibility into events               | SIEM ingestion, audit logs, telemetry dashboards    |
| Human Role       | Stick figure  | Manual actor interacting with system | Incident responders, SRE on-call, SOC analyst       |

This table mirrors the layout shown in “Screenshot 2025-11-14 121230.png” to ensure the
Examples column expands while Description stays moderate and dividers align with column
boundaries.
