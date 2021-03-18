# CS223_Final_Project

For our final project, Allison Z and I have built a farming simulation game 
called ElmVale. We used Elm Canvas to display the game graphics and wrote our
own functions for clickable interactions with the Canvas environment.

Game Description

    The object of the game is to earn money by harvesting crops. 

    The player starts out with a small initial sum of money. They unlock a plant by
    clicking on the initial price, wait for the plant to grow (which is displayed by the progress bar), then click on the plant icon to harvest it. Harvesting the plant will give the player money, which they can use to buy or unlock more plants.

Module Summary

Main
    The Main handles the standard MVC functions.

Model
    This module exposes the game model (moved out of Main so it can be imported by other modules).

Button
    This module contains all our functions that handle clicking. Because Canvas does not support click handling on individual renderables, we wrote our own functions to create buttons that recognize user clicks. 

Msg
    This module exposes our Msg type.

Page
    This module exposes the Page type, which we implemented in anticipation of multiple game pages. The current version of our game only supports one game page (the farm), but this type would allow us to create multiple game pages and switch between them.

Plants
    This module exposes our Plant type and contains all information inherent to each type of plant (name, initial price, upgrade costs, quantity, selling price, etc.)They also contain the functions that handle updates when a game element is clicked.

ViewHelpers
    This module contains all the functions that handle image rendering and text display. The functions in this module are called by "view" in the Main.

Window
    This module exposes types that store information on window and plot sizes (which are initialized in Main).


