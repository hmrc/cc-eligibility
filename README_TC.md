# TC ELIGIBILITY

The TC Eligibility micro-service will accept the parent details and children details as input request in json format and returns whether the scheme, parent and/or children are eligible to apply in a json format.


* **Data Parameters**

  `Data structure for input json:`

  ```javascript
  {
    payload : {
      taxYears : [
        {
          from : [LocalDate],
          until : [LocalDate],
          claimants : [
            {
              hours : [Double] = 0.00,
              isPartner : [Boolean] = false,
              totalIncome : [BigDecimal] = 0.00,
              previousTotalIncome : [BigDecimal] = 0.00,
              disability : {
                disabled: [Boolean] = false,
                severelyDisabled: [Boolean] = false
              }
            }
          ],
          children : [
            {
              id : [Short],
              name : Option[String],
              childcareCost : [BigDecimal] = 0.00,
              childcareCostPeriod : [Enumeration of String] {
                Week,
                Fortnight,
                Month,
                3 month,
                Year,
                INVALID
              },
              dob : [LocalDate],
              disability : {
                disabled: [Boolean] = false,
                severelyDisabled: [Boolean] = false
              },
              education : Option {
                inEducation: [Boolean] = false,
                startDate: [LocalDate]
              }
            }
          ]
        }
      [
    }
  }
  ```

`Data structure for output json:`

```javascript
  {
   eligibility: {
      tc: {
       eligible: [Boolean] = false,
       taxYears: [
         {
           from: [LocalDate],
           until: [LocalDate],
           houseHoldIncome: [BigDecimal],
           periods: [
             {
               from: [LocalDate],
               until: [LocalDate],
               householdElements: {
                 basic: [Boolean] = false,
                 hours30: [Boolean] = false,
                 childcare: [Boolean] = false,
                 loneParent: [Boolean] = false,
                 secondParent: [Boolean] = false,
                 family: [Boolean] = false
               },
               claimants: [
                 {
                   qualifying: [Boolean] = false,
                   isPartner: [Boolean] = false,
                   claimantElements: {
                     disability: [Boolean] = false,
                     severeDisability: [Boolean] = false
                   }
                 }
               ],
               children: [
                 {
                   id: [Short],
                   name: Option[String],
                   childcareCost: [BigDecimal],
                   childcareCostPeriod: [Enumeration of String] {
                      Week,
                      Fortnight,
                      Month,
                      3 month,
                      Year,
                      INVALID
                    },
                   qualifying: [Boolean] = false,
                   childElements:
                    {
                     child: [Boolean] = false,
                     youngAdult: [Boolean] = false,
                     disability: [Boolean] = false,
                     severeDisability: [Boolean] = false,
                     childcare: [Boolean] = false,
                    }
                 }
               ]
             }
           ]
         }
       ]
     },
     tfc: null,
     esc: null
     }
  }
```

* **Success Response:**

  **Code:** 200 <br />
  **Content:**

      {
        "eligibility": {
           "tc": {
             "eligible": true,
             "taxYears": [
               {
                 "from": "2015-08-27T00:00:00",
                 "until": "2016-04-06T00:00:00",
                 "houseHoldIncome": 0.00,
                 "periods": [
                   {
                     "from": "2015-08-27T00:00:00",
                     "until": "2016-04-06T00:00:00",
                     "householdElements": {
                       "basic": false,
                       "hours30": false,
                       "childcare": false,
                       "loneParent": false,
                       "secondParent": false,
                       "family": false
                     },
                     "claimants": [
                       {
                         "qualifying": true,
                         "isPartner": false,
                         "claimantElements": {
                           "disability": false,
                           "severeDisability": false
                         }
                       }
                     ],
                     "children": [
                       {
                         "id": 0,
                         "name": "Paul",
                         "childcareCost": 3000.00,
                         "childcareCostPeriod": "Month",
                         "qualifying": false,
                         "childElements":
                         {
                           "child": false,
                           "youngAdult": false,
                           "disability": false,
                           "severeDisability": false,
                           "childcare": false
                         }
                       }
                     ]
                   }
                 ]
               }
             ]
           },
           "tfc": null,
           "esc": null
           }
        }


* **Error Response:**

  **Code:** 400 <br />
  **Content:**

    ```javascript
      {
        "status": 400,
        "errors":
        [
           {
             "path" : "/tc",
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

* **Sample Input Request for TC:**

   ```javascript
   {
   "payload": {
     "taxYears": [
       {
         "from": "2015-08-27T18:46:17",
         "until": "2016-04-06T00:00:00",
         "claimants": [
           {
             "hours": 30.00,
             "isPartner": false,
             "totalIncome": 0.00,
             "previousTotalIncome": 0.00,
             "disability": {
               "disabled": false,
               "severelyDisabled": false
             }
           }
         ],
         "children": [
           {
             "id": 0,
             "name": "Adam",
             "childcareCost": 3000.00,
             "childcareCostPeriod": "Month",
             "dob": "2013-05-12T18:46:17",
             "disability": {
               "disabled": false,
               "severelyDisabled": false
             },
             "education": {
               "inEducation": true,
               "startDate": "2013-05-12T18:46:17"
             }
           }
         ]
       }
     ]
   }
  }
  ```

 * **Sample Output Response for TC:**

 ```javascript
  {
  "eligibility": {
     "tc": {
       "eligible": true,
       "taxYears": [
         {
           "from": "2015-08-27T00:00:00",
           "until": "2016-04-06T00:00:00",
           "houseHoldIncome": 0.00,
           "periods": [
             {
               "from": "2015-08-27T00:00:00",
               "until": "2016-04-06T00:00:00",
               "householdElements": {
                 "basic": false,
                 "hours30": false,
                 "childcare": false,
                 "loneParent": false,
                 "secondParent": false,
                 "family": false
               },
               "claimants": [
                 {
                   "qualifying": true,
                   "isPartner": false,
                   "claimantElements": {
                     "disability": false,
                     "severeDisability": false
                   }
                 }
               ],
               "children": [
                 {
                   "id": 0,
                   "name": "Paul",
                   "childcareCost": 3000.00,
                   "childcareCostPeriod": "Month",
                   "qualifying": false,
                   "childElements":
                   {
                     "child": false,
                     "youngAdult": false,
                     "disability": false,
                     "severeDisability": false,
                     "childcare": false
                   }
                 }
               ]
             }
           ]
         }
       ]
     },
     "tfc": null,
     "esc": null
     }
  }
 ```

* **Notes**
 
