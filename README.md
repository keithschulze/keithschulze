# keithschulze.com

This is the code for my [Hakyll](https://jaspervdj.be/hakyll/)-based side.

## Requirements

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- [npm](https://www.npmjs.com/) or [yarn](https://yarnpkg.com/)


## Building the site

1. Clone this repository:

  ```
  $ git clone https://github.com/keithschulze/keithschulze.github.io.git
  $ cd keithschulze.github.io/
  ```

2. Install front-end dependencies using `yarn` (or `npm`):

  ```
  $ yarn install
  ```

3. Build the Hakyll site executable:

  ```
  $ stack build --fast
  ```

4. Build the site using Hakyll:

  ```
  $ stack exec site build
  ```
