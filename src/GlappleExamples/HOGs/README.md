# HOGs (High-Order Games)

You can think of HOGs (High-Order Games) in the same way as HOCs (High-Order Components) in web app development. HOGs is simply a function that takes a certain `GameSpecM` and returns a `GameSpecM`. Internally, HOGs behaves as a parent component and the given `GameSpecM` is executed as a child game.

> Note: It is recommended that HOGs be destroyed when the child game is destroyed; if you are creating many child games with functions in HOGs, you can keep the memory usage constant by destroying HOGs as the child games are destroyed.

## HOGs Example (in this directory)
- `Fixer  :: forall s g i o. Transform -> GameSpecM s g i o -> GameSpecM s { gameId :: GameId s i } i o`

Fix the game with the given Transform