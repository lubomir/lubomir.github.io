---
title: Simulating an elevator
tags: Haskell, simulation, model
---

Recently I lived in a building with twelve floors and a very dumb elevator.
Multiple times have I stood in front of an elevator and watched as it went by
repeatedly refusing to stop and let me on.

There were two elevators, each equipped with a single button that should tell
the elevator to come to your floor. However, if the elevator is currently going
somewhere, it will ignore your request. You have to wait for it to stop and
press the button again. Should someone press it faster, you are out of luck and
have to wait for next try. 

Obviously, the two elevators do not communicate in any way, so in fact you
gamble with two elevators at the same time.

This system seemed very stupid to me, so I decided to create a simple model to
verify how much better it would be if the elevator was somewhat smarter.


## The model

So let's model a building with a single elevator. The simulation will run in
turns. At the beginning of each turn, a person can appear at each floor with
some probability. All people want to go to the lowest floor (think busy
morning). Next, the elevator must decide whether to go up, down, load or unload
people or wait.

We will test three different elevator logics. In the simulation, the elevator
is represented by a `Haskell` function with type `Building -> Decision`, where
`Building` is a data type with information about waiting people and actual
position of the elevator.

Note that even the simplest elevator in the simulation is still more elaborate
than the one in my building. To simulate that, it would be necessary to choose
floor to pick people up at random. This would require the elevator logic to
retain some state between each call, which currently is not possible.


## The elevators

The simplest elevator is called `dumbElevator`, but none-the-less it still is a
bit smarter that the one in my former building. This elevator stores each
request in a queue and works in a first-come-first-served fashion.

```Haskell
dumbElevator b
  | hasPeople b = if inBasement b then Unload else GoDown
  | otherwise = case (== b ^. curPosition) `fmap` (b ^. queue ^? _head) of
        Just True  -> Load
        Just False -> GoUp
        Nothing    -> Wait
```

If there are people on the elevator, it will take them to the basement and
unload. Otherwise it will go to the next floor in the queue and load all the
people there.

We will improve this elevator with slightly nicer behavior in `niceElevator`:
when it goes down through a floor with people, it will stop and let on.

```Haskell
niceElevator b
  | hasPeople b = if inBasement b
                    then Unload
                    else if peopleOnCurrentFloor b then Load else GoDown
  | otherwise = dumbElevator b
```

The last modeled elevator behaves the same as `niceElevator` when going down.
Where it differs is that when it decides where to start loading people, it
ignores the order of requests and goes to the highest floor with waiting
people.

```Haskell
smartElevator b
  | hasPeople b = niceElevator b
  | any (> b ^. curPosition) (b ^. queue) = GoUp
  | peopleOnCurrentFloor b = Load
  | otherwise = Wait
```

It does not preserve any state. In each turn, it will check if there is a floor
above it with people. If so, it will go for them. If not and there are people
on current floor, it lets them on, otherwise it will wait. Note that a
situation when people are waiting below the elevator can never happen, as the
elevator goes up only if it can pick someone up there.


## Results

It should not be a surprise that the dumb elevator does not behave very well.
The metric that I measured is the average waiting time. Every time a person got
on the elevator, their waiting time (current time minus the appearance of this
person) was recorded. This way we can determine how bad the elevator is. The
number itself does not say much, but a comparison of different elevators does.

The first graph displays the average waiting time with regards to the
probability of a person appearing.

![In a 5 floor building](/images/elevator/wait-vs-prob-5.png)

If the building has more floors, the trend remains the same. The wait time for
*dumb* elevator goes much higher than for the other two elevators. However, the
additional cunning of *smart* elevator does not seem to bring much.

![In a 10 floor building](/images/elevator/wait-vs-prob-10.png)

It seems from the graphs that at certain point the elevators can no longer deal
with the amount of people appearing in the building. The simulation was run for
10000 turns and people who were never picked up do not have any effect on the
average time, thus the upper bound on the wait time.

It might also be interesting to have a look at the relation between waiting
time and number of floors.

![For probability 5 %](/images/elevator/wait-vs-floors-5.png)

Interestingly, the probability of people appearing has pretty much no influence
on the results.

![For probability 15 %](/images/elevator/wait-vs-floors-15.png)


## Conclusion

There is one positive result: I don't have to live a with a dumb elevator
anymore. The simulation confirmed that a slight improvement in the logic –
letting people get on the elevator when it is going down – can have
significant impact on average waiting time.

The code used for this is available at
[GitHub](https://github.com/lubomir/elevator).
