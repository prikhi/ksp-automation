# Kerbal Space Program Automation

[![Build Status](https://travis-ci.org/prikhi/ksp-automation.svg?branch=master)](https://travis-ci.org/prikhi/ksp-automation)

A CLI KSP Automation program & library that utilizes the [kRPC][krpc] mod.

Currently in early alpha - the only current supported program is achieving
Low-Kerbin Orbit from the Kerbal Space Center Launchpad.

![The User Interface of the KSP Automation Program, Showing a Mission List, Parameters, Logs, and Options](http://bugs.sleepanarchy.com/projects/ksp-automation/repository/revisions/master/entry/screenshot.png "KSP Automation Screenshot")


## Goals

The future will include launch/abort buttons, more missions, tweakable
parameters for missions, global automation(like automatically staging whenever
fuel in stage is depleted).

Eventually this could be expanded into a "Mission Builder" where you can select
a vessel & tweak/assemble a list of tasks into larger missions. Something that
would let you create KSC -> LKO -> Dock at Kerbin Station -> Dump Fuel ->
Kerbin Landing. Some sort of management system for these custom missions.

A special interface for GHCi would be cool. I'd imagine something that hides
the connection & log arguments like:

    initializeConnection 50000 50001
    lowKerbinOrbit 80000
    setTarget "KerbStation"
    rendezvous
    activateStage
    reEnterKerbin


## Build / Run

There are currently no downloadable releases, you will need to build the
application yourself. The build process requires [Haskell stack][stack]:

    sudo pacman -S stack
    stack build
    stack exec ksp-automation

The application currently assumes that kRPC is running on ports 50000 & 50001
of the local machine.

Use `j`/`k` to scroll the mission list & log, `Enter` to start a mission,
`Ctrl-n` & `Ctrl-p` to cycle through the sections.


## Contribute

There are many `TODO`s littered throughout the source, any of these can be
tackled. Ideally they would be moved into Github issues.

For changes with a larger scope, please open an issue for discussion first.


## License

GPL-3.0


[krpc]: https://github.com/krpc/krpc/
[stack]: https://github.com/commercialhaskell/stack/
