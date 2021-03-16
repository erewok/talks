# Talks

These are generally built with markdown to turn into revealJS or remark presentations.

## How to Generate

This repo uses [`reveal-md`]() in order to generate reveal.js presentations from markdown files.

To generate presentations from markdown files, first install `reveal-md`:

```sh
$ npm install -g reveal-md
~/.nvm/versions/node/v10.16.0/bin/reveal-md -> ~/.nvm/versions/node/v10.16.0/lib/node_modules/reveal-md/bin/reveal-md.js
```

After that, you can generate a presentation like this:

```sh
$ reveal-md path/to/my/slides.md --theme solarized
Reveal-server started at http://localhost:1948
```