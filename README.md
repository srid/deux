# deux

Are graphs and mind maps underrated?

A playground to create the most awesome'est PIM ever.

This started out by using Dhall as the underlying data format, but I've become somewhat dissatisfied with this approach. I am currently exploring the use of Postgres, and also limiting the scope to tree instead of graph. See [motif](https://github.com/srid/motif).

## Development

See
[project-development.md](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md).

### ghcid based

Auto-reload the frontend server using ghcid:

```
./ghcid-frontend
```

Do the same for the backend.

```
./ghcid-backend-dhall
```

Now access the frontend at port 3000 (it uses the backend API from port 3001).

## hoogle

Run the documentation server using:

```
./hoogle  # Port 8081
```
