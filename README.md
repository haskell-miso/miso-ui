:ramen: 🎨 miso-ui
====================

`miso-ui` is a component library that uses [Basecoat](https://basecoatui.com/) to deliver [ShadCN](https://ui.shadcn.com) styling. It uses [Tailwind CSS](https://tailwindcss.com/) utility classes under the hood.

View live [here](https://miso-ui.haskell-miso.org)

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

This comes with a GitHub action that builds and auto hosts the example.
