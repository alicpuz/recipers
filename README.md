# recipers

Simple web application for adding and searching recipes written in Haskell.

## Technologies
- Haskell
- Threepenny-GUI
- PostgreSQL

## Features
- adding recipes
- browsing recipes
- searching recipes

## Purpose
This project was created as part of the university course:
**Functional Programming**.

## Database setup

Before running the application, create a PostgreSQL database named:

```text
recipes
```

Make sure PostgreSQL is running locally.

You may also need to update database credentials in `Connection.hs`.

## Running the project

```bash
cabal run
```

Application runs on:

```text
http://127.0.0.1:8023
```
