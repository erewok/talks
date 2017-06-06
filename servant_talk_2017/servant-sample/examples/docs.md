## POST /counter-multiplier/:mult

#### Authentication



Clients must supply the following data


#### Captures:

- *mult*: (integer) to multiply our counter by

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"value":5}
```

## POST /counter-post

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"value":5}
```

## GET /counter-queryparam

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Some-Header** HTTP header.

#### GET Parameters:

- sortby
     - **Values**: *val, ...*
     - **Description**: A dummy query param we're not even using.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"value":5}
```

## POST /counter-reset-post

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"value":5}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"value":5}
```

