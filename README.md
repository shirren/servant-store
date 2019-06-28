# Example Servant Application

The goal of this project is to share a template for writing a RESTful API in Haskell using the Servant library for the web layer and the Beam library for the data access layer and to make authoring Haskell code in the web realm as accessible as possible. However this project assumes some base Haskell knowledge.

External contributions are more than welcome. Please get in contact via this projects issues tracker if you would like to help out.

## Getting Started

This application has the following pre-requisites;

* Haskell 8.4.3+
* Cabal 2.2.0.0+
* Postgres 11+

Ensure the application compiles with the following command `cabal new-build`. Once the application compiles successfully (which it should do) then run the migrations.

### Migrations

This example application currently does not support migrations. It is on my todo list. Till migrations are available the following `CREATE TABLE` sql statements need to be run in sequential order. But to begin first create the database in Potgres with the following command;

```SQL
CREATE DATABASE store_dev;
```

```SQL
CREATE TABLE users (
    id SERIAL
  , email VARCHAR NOT NULL
  , first_name VARCHAR NOT NULL
  , middle_name VARCHAR
  , last_name VARCHAR NOT NULL
  , perma_id VARCHAR default md5(random()::text)
  , PRIMARY KEY(id)
);
```
```SQL
CREATE TABLE products (
    id SERIAL
  , description VARCHAR NOT NULL
  , price INT NOT NULL
  , perma_id TEXT default md5(random()::text)
  , PRIMARY KEY(id)
);
```

```SQL
-- To stick with beams conventions we use the __ notation for foreign keys
-- if you would like to use the more conventional approach of a single underscore
-- then we need to overwrite the default settings.
CREATE TABLE orders (
    id SERIAL
  , user__id INTEGER REFERENCES users(id)
  , product__id INTEGER REFERENCES products(id)
  , perma_id TEXT default md5(random()::text)
  , PRIMARY KEY(id)
);
```

### Running The Application

To run the application use the command `cabal new-run`. The application has been configured to debug to STDOUT as requests are made both at the Web layer, and the DB layer. You will see which routes are processing requests, and what the executed queries look like. The application cannot be currently configured and will most likely not connect to your Postgres database because the connection string requires a username and password. The connection string to connect to Postgres is located in the `Data.DB` module in the `getConnection` function.

## Domain Model

The domain model is intentionally simple and made familiar on purpose. A simple store with users, products and orders. Each of the key domain elements are separated into a namespace of its own. In each module (e.g. users) you will find three modules; API, Data, Resources and Types. API deals with the Http layer of the module, constructed using the Servant library. The Data layer deals persistence functions, constructed using the Beam library. The Types layer provides the core type definitions for the namespace.

## Resource Model

### GET a single resource

This example application does not serialise internal/beam types to JSON. This is to avoid the exposition of the relational model backing the system. Instead the application creates resource representations for each of the domain types. So in our application we have a resource type for User, Product and Order. These types are serialised to JSON using the [JSON API](http://jsonapi.org) format specification. So for example suppose we are returning an instance of a single resource like user, we would issue a HTTP GET to the url `/users/:id`, we then find a user with the given identifier represented in the URL as :id. When we find the user what we have is a Record of type `User`. we then create a Record of type `Document UserResource` with the function [mkUserResource](https://github.com/shirren/servant-store/blob/master/src/Users/Api.hs#L112), and then pass this type to the function `mkSimpleDocument`. This pattern is repeated whenever we return a single resource. In the next section we look at how we return a collection of resources.

```
getUser :: Text -> Handler (Document UserResource)
getUser uId = do
  result <- liftIO $ findById uId
  case result of
    Just user -> pure $ mkSimpleDocument [mkUserResource user]
    _         -> throwError err404
```

### GET multiple resources

Following on from the last section we now present how to retrieve a collection of `User` resources. To retrieve a collection of resources we issue a HTTP GET to the URL `/users` following RESTful conventions. we then retrieve a `page` ([Pagination](#Pagination)) of users. TO BE CONT... 


## Ordering

This projects demonstrates how to implement sorting using the Beam API. All the uses can naturally be found in the data modules of each of the higher level modules (i.e. Products and Users). In the products data module we demonstrate how to sort on the [product description column in ascending order](https://github.com/shirren/servant-store/blob/master/src/Products/Data.hs#L29). In the users data module we demonstrated how to sort on 2 columns, [one in ascending order, and the other in descending order](https://github.com/shirren/servant-store/blob/master/src/Users/Data.hs#L30).

## Pagination

## Checklist

The following checklist illustrates some of the concepts covered by this prototype.

- [x] Nullable columns
- [x] Autogenerated values (primary key & resource identifiers)
- [x] Foreign key associations
- [x] Find all
- [x] Find by id
- [x] Find with associations
- [x] Insert
- [x] Update
- [x] Delete
- [x] Ordering
- [x] Limits
- [x] Offsets
- [ ] Migrations
- [ ] Custom column names
- [x] JSON Api
- [ ] Authorisation (OAuth2)
- [ ] NEST configuration
- [ ] Tests
- [ ] Error Handling

## Feedback

