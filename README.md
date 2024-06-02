# parse-json

This is a simple JSON parser implemented in Haskell.

- `src/Lexical.hs` Module for lexical analysis (turning a raw content into tokens for further parsing) 
- `src/Parse.hs` Module for parsing and API

You can run `app/Main.hs` to test it in console:

```
>> stack build
>> stack exec parse-json-exe
Please provide filename:
example.json
Parse result: 
[
    JsonObject [("first_name", [JsonString "John"])],
    JsonObject [("last_name", [JsonString "Smith"])],
    JsonObject [("is_alive", [JsonBool True])],
    JsonObject [("age", [JsonNumber 27.0])],
    JsonObject [
        ("address", [
            JsonObject [("street_address", [JsonString "21 2nd Street"])],
            JsonObject [("city", [JsonString "New York"])],
            JsonObject [("state", [JsonString "NY"])],
            JsonObject [("postal_code", [JsonString "10021-3100"])]
        ])
    ],
    JsonObject [
        ("phone_numbers", [
            JsonObject [("type", [JsonString "home"])],
            JsonObject [("number", [JsonString "212 555-1234"])],
            JsonObject [("type", [JsonString "office"])],
            JsonObject [("number", [JsonString "646 555-4567"])]
        ])
    ],
    JsonObject [("children", [JsonString "Catherine", JsonString "Thomas", JsonString "Trevor"])],
    JsonObject [("spouse", [JsonNull])]
]
```