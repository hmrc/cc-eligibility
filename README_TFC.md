# TFC ELIGIBILITY

The TFC Eligibility micro-service will accept the parent details and children details as input request in json format and returns whether the scheme, parent and/or children are eligible to apply in a json format.

* **Data Parameters**

  `Data structure for input json:`

```javascript
  {
    payload: {
      tfc: {
          from : [LocalDate],
          numberOfPeriods : [short],
          claimants: [
            {
              liveOrWork : [Boolean] = false,
              totalIncome: [BigDecimal] = 0.00,
              earnedIncome: [BigDecimal] = 0.00,
              hoursPerWeek: [Double] = 0.00,
              isPartner : [Boolean] = false,
              disability:{
                disability: [Boolean] = false,
                severeDisability: [Boolean] = false
              },
              otherSupport: OtherSupport {
                disabilityBenefitsOrAllowances: [Boolean] = false,
                severeDisabilityBenefitsOrAllowances: [Boolean] = false,
                incomeBenefitsOrAllowances: [Boolean] = false,
                carersAllowance: [Boolean] = false,
              }
            }
          ],
          children: [
            {
                id: [short],
                name: Option[String],
                dob: [LocalDate],
                disability: {
                disabled: [Boolean] = false,
                severelyDisabled: [Boolean] = false,
              }
            }
          ]
      }
    }
  }
```

`Data structure for output json:`

```javascript
{
    eligibility: {
      tfc: {
           from : [LocalDate],
        until : [LocalDate],
        householdEligibility: [Boolean] = false,
        periods: [
              {
             from: [LocalDate],
           until: [LocalDate],
           periodEligibility: [Boolean] = true,
           claimants: [
                  {
                    qualifying: [Boolean] = false,
                    isPartner: [Boolean] = false,
                    failures: List[String]
                  }
                ],
           children: [
                  {
                    id: [short],
                    name: Option[String],
                    qualifying: [Boolean] = false,
                    from: [LocalDate],
                    until: [LocalDate],
                    failures: List[String]
                  }
                ]
              }
            ]
          }
      }
}
```
* **Success Response:**
  **Code:** 200 <br />
  **Content:**
```javascript
  {
    "payload": {
      "tfc": {
        "from": "2016-08-27",
        "numberOfPeriods": 3,
        "claimants": [
          {
            "hoursPerWeek": 30.00,
            "liveOrWork": true,
            "isPartner": false,
            "totalIncome": 0.00,
            "earnedIncome": 0.00,
            "disability": {
              "disabled": false,
              "severelyDisabled": false
            },
            "otherSupport": {
              "disabilityBenefitsOrAllowances":false,
              "severeDisabilityBenefitsOrAllowances":false,
              "incomeBenefitsOrAllowances":false,
              "carersAllowance":false
            }
          }
        ],
        "children": [
          {
            "id": 0,
            "name": "Venky",
            "childcareCost": 3000.00,
            "childcareCostPeriod": "Month",
            "dob": "2014-05-12",
            "disability": {
              "disabled": false,
              "severelyDisabled": false
            }
          }
        ]
      }
    }
  }
```
* **Error Response:**

  **Code:** 400 <br />
  **Content:**

    ```javascript
      {
        "status": 400,
        "errors":
        [
           {
             "path" : "/tfc",
             "validationErrors" :
             [
               {
                "message": "Very Bad Thing Happened",
                "args": [42]
               },
               {
                "message": "Not So Bad Thing Happened",
                "args": ["Error"]
               }
             ]
           }
        ]
      }
    ```

  **Code:** 500 <br />
  **Content:**

   ```javascript
     {
       "status": 500,
       "errors": ["Error while generating JSON response"]
      }
   ```

* **Sample Input Request for TFC**
   ```javascript
   {
     "payload": {
           "tfc": {
         "from": "2016-08-27",
         "numberOfPeriods": 1,
         "claimants": [
           {
             "hoursPerWeek": 6.50,
             "liveOrWork": false,
             "isPartner": false,
             "totalIncome": 151000.00,
             "earnedIncome": 0.00,
             "disability": {
               "disabled": false,
               "severelyDisabled": false
             },
             "otherSupport": {
               "disabilityBenefitsOrAllowances":false,
               "severeDisabilityBenefitsOrAllowances":false,
               "incomeBenefitsOrAllowances":false,
               "carersAllowance":false
             }
           }
         ],
         "children": [
           {
             "id": 0,
             "name": "Venky",
             "childcareCost": 3000.00,
             "childcareCostPeriod": "Month",
             "dob": "2014-05-12",
             "disability": {
               "disabled": false,
               "severelyDisabled": false
             }
           }
         ]
       }
     }
   }
  ```
 * **Sample Output Response for TFC:**
 ```javascript
  {
    "eligibility": {
      "tc": null,
      "esc": null,
      "tfc": {
        "from": "2016-08-27",
        "until": "2016-11-27",
        "householdEligibility": false,
        "periods": [
          {
            "from" : "2016-08-27",
            "until" : "2016-11-27",
            "periodEligibility" : false,
            "claimants" : [
              {
                "qualifying" : false,
                "isPartner" : false,
                "failures" : []
              }
            ],
            "children" : [
              {
                "id" : 0,
                "name" : "Venky",
                "qualifying" : true,
                "from" : "2016-08-27",
                "until" : "2016-11-27",
                "failures" : []
              }
            ]
          }
        ]
      }
    }
  }
 ```
* **Notes**
