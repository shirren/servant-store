# Example Servant Application

The goal of this project is to share a template for writing an API in Haskell using the Servant library for the web layer and the Beam library for the data access layer and to make authoring Haskell code in the web realm as accessible as possible. The layout of the code will be described in this layout, but the intention will be to document the code and provide as much detail as possible. This project is currently in its infancy. External contributions are more than welcome. Please get in contact with via this projects issues tracker if you would like to help out.

## Getting Started

This application has the following pre-requisites;

* Haskell 8.4.3+
* Cabal 2.2.0.0+
* Postgres 11+

Ensure the application compiles with the following command `cabal new-build`. Once the application compiles successfully (which it should do) then run the migrations.

### Migrations

This example application currently does not support migrations. It is on my todo list. Till migrations are available the following `CREATE TABLE` sql statements need to be run in sequential order.

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

To run the application use the command `cabal new-run`. The application has been configured to debug to STDOUT as requests are made both at the Web layer, and the DB layer. You will see which routes are processing requests, and what the executed queries look like.

## Domain Model

The domain model is intentionally simple and made familiar on purpose. A simple store with users, products and orders. Each of the key domain elements are separated into a namespace of its own. In each module (e.g. users) you will find three modules API, Data and Types. API deals with the Http layer of the module, constructed using the Servant library. The Data layer deals persistence functions, constructed using the Beam library. The Types layer provides the core type definitions for the namespace.

## Checklist

The following checklist illustrates some of the concepts covered by this prototype.

- [x] Nullable columns
- [x] Autogenerated values (primary key & perma id)
- [x] Foreign key associations
- [x] Find all
- [x] Find by id
- [x] Find with associations
- [ ] Update
- [ ] Insert
- [ ] Delete
- [ ] Ordering
- [x] Limits
- [x] Offsets
- [ ] Migrations
- [ ] Custom column names
- [ ] JSON Api
- [ ] Authorisation (OAuth2)
- [ ] NEST configuration