# ESC ELIGIBILITY

The ESC Eligibility micro-service will accept the parent details and children details as input request in json format and returns whether the scheme, parent and/or children are eligible to apply in a json format.


* **Data Parameters**

  `Data structure for input json:`

  ```javascript
  {
    payload: {
      taxYears: [
        {
      from : [LocalDate],
          until : [LocalDate],
          claimants: [
            {
              isPartner : [Boolean] = false,
              employerProvidesESC: [Boolean] = false
              elements: {
                vouchers: [Boolean] = false
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
      ]
    }
  }
  ```

`Data structure for output json:`

```javascript
{
    eligibility: {
      esc: {
        taxYears: [
          {
   	    from : [LocalDate],
            until : [LocalDate],
            periods: [
              {
   		from : [LocalDate],
        	until : [LocalDate],
                claimants: [
                  {
                    qualifying: [Boolean] = false,
                    isPartner: [Boolean] = false,
                    eligibleMonthsInPeriod: [Int],
                    elements: {
                    vouchers: [Boolean] = false,
                    },
                    failures: List[String]
                  }
                ],
                children: [
                  {
                    id: [short],
                    name: Option[String],
                    qualifying: [Boolean] = false,
                    failures: List[String]
                  }
                ]
              }
            ]
          }
        ]
      },
      tc: null,
      tfc: null
    }
}
```

* **Success Response:**

  **Code:** 200 <br />
  **Content:**

```javascript
  {
    "eligibility": {
      "esc": {
        "taxYears": [
          {
            "from": "2015-08-27T00:00:00",
            "until": "2016-04-06T00:00:00",
            "periods": [
              {
                "from": "2015-08-27T00:00:00",
                "until": "2016-04-06T00:00:00",
                "claimants": [
                  {
                    "qualifying": true,
                    "isPartner": false,
                    "eligibleMonthsInPeriod": 0,
                    "elements": {
                      "vouchers": false
                    },
                    "failures": []
                  }
                ],
                "children": [
                  {
                    "id": 0,
                    "name": "Adam",
                    "qualifying": false,
                    "failures": []
                  }
                ]
              }
            ]
          }
        ]
      },
      "tc": null,
      "tfc": null
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
             "path" : "/esc",
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

* **Sample Input Request for ESC:**

   ```javascript
    {
      "payload": {
        "taxYears": [
          {
            "from": "2015-08-27T18:46:17",
            "until": "2016-04-06T00:00:00",
            "claimants": [
              {
                "isPartner": false,
                "employerProvidesESC": true,
                "elements": {
                  "vouchers": true
                }
              }
            ],
            "children": [
              {
                "id": 0,
                "name": "Adam",
                "dob": "1998-08-31T18:46:17",
                "disability": {
                  "disabled": true,
                  "severelyDisabled": false
                }
              }
            ]
          }
        ]
      }
    }
  ```

 * **Sample Output Response for ESC:**

 ```javascript
  {
      "eligibility": {
        "esc": {
          "taxYears": [
            {
              "from": "2015-08-27T00:00:00",
              "until": "2016-04-06T00:00:00",
              "periods": [
                {
                  "from": "2015-08-27T00:00:00",
                  "until": "2016-04-06T00:00:00",
                  "claimants": [
                    {
                      "qualifying": true,
                      "isPartner": false,
                      "eligibleMonthsInPeriod": 0,
                      "elements": {
                        "vouchers": false
                      },
                      "failures": []
                    }
                  ],
                  "children": [
                    {
                      "id": 0,
                      "name": "Adam",
                      "qualifying": false,
                      "failures": []
                    }
                  ]
                }
              ]
            }
          ]
        },
        "tc": null,
        "tfc": null
      }
  }
 ```
* **Notes**
