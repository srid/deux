# deux

Are graphs and mind maps underrated?

A playground to create the most awesome'est PIM ever.

# Development

See
[project-development.md](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md).

# ghcid based

Auto-reload the frontend server using ghcid:

```
nix-shell -A shells.ghc --run "cd frontend && ghcid -W -c 'cabal new-repl frontend' -T'Main.main'"
```

Do the same for the backend.

Now access the frontend at port 3000 (it uses the backend API from port 3001).
