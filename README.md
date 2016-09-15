**CC Eligibility**
----
![Build status](https://ci-dev.tax.service.gov.uk/buildStatus/icon?job=cc-eligibility)

The Childcare Eligibility will help parents to quickly self-assess their and child's eligibility from Tax-Free Childcare, Tax Credits, Employer-Supported Childcare and <br />
understand how much support they could get. This will help parents to make a decision on which scheme will best suit their needs.

The Eligibility micro-service common to all the schemes (Tax, Credit, Tax-Free Childcare, Employee Supported Childcare) of childcare calculator.

The service will accept the parent details and children details as input request in json format and returns whether the scheme, parent and/or children are eligible to apply in
a json format.


* **Endpoint URLs**

  * **TC  :**  /cc-eligibility/tax-credits/eligibility

  * **ESC :**  /cc-eligibility/employer-vouchers/eligibility

  * **TFC :**  /cc-eligibility/tax-free-childcare/eligibility



* **Port Number**

  * **CC Eligibility :** 9375



* **Method:**

  All requests are of type `POST`


For TC Eligibility documentation, please click [here](README_TC.md).

For ESC Eligibility documentation, please click [here](README_ESC.md).

For TFC Eligibility documentation, please click [here](README_TFC.md).


License
---

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
