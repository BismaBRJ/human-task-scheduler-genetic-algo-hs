# Human Task Scheduler

A web app as a static website built using Reflex-DOM, a Haskell web framework following the pure FRP *(functional reactive programming)* paradigm, in which anyone can input

- their working hours (calendar),

- their full hours (occupied slots of the calendar), as well as

- their tasks, including deadlines and estimated completion time

following the specified format, and the web app will automagically compute recommended times to work on those tasks, subject to humane constraints such as the obvious completion-before-deadline.

As the name of the project suggests, the genetic algorithm will eventually be fully implemented to do so.

## Online usage

Try it: <https://bismabrj.github.io/human-task-scheduler-genetic-algo-hs/>

## Offline usage

Download the "webapp" folder of this repository, and open `index.html` (leave all other files untouched).

This web app is a standalone static webpage, so you can put it anywhere you like.

## Compiling from scratch

### Required installations

1. Install Haskell, such as by using `GHCup`: <https://www.haskell.org/downloads/>

    You may need about 6--10 GB or more for this.

2. Install the [Nix package manager](https://github.com/NixOS/nix) (no need to install the Nix operating system).

    It should be lightweight.

3. Install Reflx-DOM via the Nix package manager, specifically by installing [Reflex Platform](https://github.com/reflex-frp/reflex-platform).

    Make sure to activate the nix caches as instructed so it would take at most a couple of hours rather than at least an entire day, and so that it wouldn't take up more space with all the intermediate compilation files. Even then, you will still need at least 20 GB of disk space free.

    **Note:** as of the time of writing this, Reflex Platform might not be supported on Windows yet. Feel free to try another Reflex-DOM setup such as [Obelisk](https://github.com/obsidiansystems/obelisk), at your own risk (of headaches).

    **Personal anecdote:** I first chose Obelisk, got headaches installing it, and got even more headaches trying to get it to work on my machine (it seems to work on everybody else's), until I eventually gave up and tried Reflex Platform second. After some more headaches until a successful run with Reflex Platform, I never looked back.

### Compilation

4. Download the "source" folder of this repo.

5. Open the "source" directory on the terminal, and run

    ```bash
    nix-shell
    ```

    It may be that you haven't fully installed Reflex Platform by this point, and if that's the case, it may take a couple of hours and it will take up about 20 GB of disk space in total, for real this time.

    If however you already have Reflex Platform fully installed before running the above command, it should only take a few minutes at most and won't actually install anything much extra (despite the alarming "unpacking from GitHub" messages).

6. Run the following command:

    ```bash
    ghcjs hts_ga_frontend.hs
    ```

    Yeah it may seem like it'll only compile the frontend but don't worry, the backend is imported in there so it will automatically be compiled along too.

    Wait for a few minutes, at most 10 (ten) minutes perhaps. A new folder will be created, one of its files being `index.html`.
    
    This new folder, in fact, is the same "webapp" folder of this repo, so there you have it!

**Note:** suppose you deleted that new, compiled folder. You'll only need to repeat steps 5 and 6 if you want it back. Unless, of course, if you also uninstalled any of the Haskell stuff...
