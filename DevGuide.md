# ShinyPsych DevGuide

## Commits and Branches
The **master** branch should evolve slowly and only be updated if the added features warrant an increased
version number (e.g. from 0.1 to 0.2), run stable and are thoroughly tested.
Developement is done on the **Dev** branch. It should also be functional at any time,
as another person might want to branch from it to create a **feature** branch.
If a feature branch is created, it should be merged back with the Dev branch when development of the feature is finished.
Smaller features can also be developed locally and can be commited directly to the Dev branch
whithout creating a feature branch first.

## Code
As Shiny can be somewhat unstable at times, we try to use every trick in the book to make R run as efficiently as possible.
However, the code should be understandable and be thoroughly commented where something might be unclear.
Variable and function names should be chosen such that they are as self-explanatory as possible, while remaining reasonably short.
