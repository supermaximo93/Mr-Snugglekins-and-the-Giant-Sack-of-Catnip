///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                  MR SNUGGLEKINS AND THE GIANT SACK OF CATNIP            ///
///                         by Max Foster                                   ///
///                   https://github.com/supermaximo93                      ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

{
   Copyright (c) 2011 Max Foster

   http://creativecommons.org/licenses/by/3.0/
}

{
 A game created for the pascalgamedevelopment.com mini competition.
The aim of the competition was to create a game with eight buttons or less, with 50 bonus points for each unused button.
There were eleven entries, and they were scored on the following criteria: Graphics & Audo; Ease of Control;
Stability & lack of bugs; Innovation & Creativity; Fun Factor. This game came fourth place, while using all eight buttons,
and came first place without button bonuses!
For full results, see here: http://www.pascalgamedevelopment.com/content.php?263-1st-PGD-Challenge-Final-Results

Uses OpenGL and SDL with the SuperMaximo GameLibrary, also created by myself. To see the latest
source code for it (and my other projects), go to my Github page at  https://github.com/supermaximo93
The version used in this game is an older version.

NOTE ABOUT OPENGL:
The OpenGL code used in this game and in the SuperMaximo GameLibrary does not use the fixed function graphics pipeline, but
instead uses the programmable shader pipeline. The fixed function pipeline has been deprecated in modern OpenGL (actually, it
has been removed completely, but a compatability profile exists in OpenGL implementations to support the old functionality).
The reason I mention this is because many tutorials online, such as the popular nehe.gamedev.net tutorials, teach the fixed function
pipeline, and therefore the OpenGL code used in this game might look alien to some people who maybe followed these tutorials.
To learn about the programmable pipeline (which I HIGHLY recommend!), I recommend the OpenGL SuperBible 5 book (although you have
to read most of it because it teaches you the author's own library at first rather than OpenGL and then eases you into the OpenGL.
You don't actually learn how to draw anything completely by yourself until the last few chapters).

}

{
/////////////////////////////////////////
/////          GAME README     //////////
/////////////////////////////////////////

NOTE: this readme was provided with the competition entry. When the game is compiled with the NO_BUTTON_LIMIT compiler
directives, use WASD to move around, and use the mouse to aim, using the left mouse button to fire. Navigate the menus
by point and click with the mouse. If the NO_BUTTON_LIMIT compiler directive is not used, then the controls outlined in
this readme are valid

BACKGROUND
==========
You play as Mr Snugglekins, who has stashed a great big sack of catnip for himself to enjoy!
Unfortunately, the government of Cattyland has put a tax on catnip, but you refuse to pay, so they send in the police; an army of spambots!

Defend your sack of catnip from the spambots for as long as possible in this 3rd person shooter adventure!

CONTROLS
========
W : move forward / move up through menus
S : move backward / move down through menus
A : strafe left
D : strafe right
J : rotate anticlockwise
L : rotate clockwise
I (AKA 'fire' or 'trigger' in game) : shoot / do action / make menu selection
Esc : go to pause menu (if in game)

THE GAME SCREEN
===============
In the top left hand corner is the health for your sack of catnip
The the top right hand corner is your score
In the bottom left hand corner is your health. If you die, you will be delayed in your defence by five seconds!
In the bottom right hand corner is your ammo, money, and 'euphoria meter' (shaped like a lightning bolt).
You build up your euphoria meter as you destroy enemies. When it reaches maximum capacity,
you get hypercharged and your running speed, firing speed and bullet damage are doubled for a period of time.
In front of your cat there is a line which is either white or red, which is to help you aim.
When the line is red then you are garunteed to hit an object.

THE SHOP
========
When you destroy enemies, you'll get some money for it. Spend your hard earned cash in the shop!
To go to the shop, go to the pause menu by pressing Esc and navigate to the 'SHOP'.
Use the W and S keys to look through the different items you can buy, and press fire (I) to buy an item.

GUNS
====
There are four guns to choose from, which can be bought from the shop or picked up as a random drop from a spambot:

Kitty Gun : Your standard gun
Akat : A hitscan gun; it hits the target instantly. Your sight line will change red when you're in range of an object and can do damage to it.
Bird Catcher : A shotgun
Anti Mech : A bazooka which reloads slowly, but does large amounts of damage and objects in the surrounding explosion
area when it hits are hurt with splash damage (including yourself!)

PICKUPS
=======
Pickups are dropped randomly by enemies.
Healthpack : A white box with a red cross. Replenishes some health
Euphoria Pack : An orange box with a black lightning bolt. Fills up some of your euphoria meter

TURRETS
=======
Turrets can be bought from the shop. When a turret is bought from the shop it is placed in the closest free space to you
in the direction you are facing. If you are standing infront of an obstacle, the turret will be built in front of the obstacle.
They automatically scan for and shoot at enemies. The Kitty Gun turret is an automatic Kitty Gun and the Akat turret is an automatic Akat.
Turrets can be upgraded by walking near them, and by pressing fire (I). You will then be shown a turret upgrade menu.
You can navigate through the options with WASD and press trigger to select an option to buy. Firepower upgrades increase the firing speed,
range upgrades increase the firing range, and health upgrades help you in case you go on a firing spree and accidentally start hurting your own turrets!
Turrets can be 'hacked'. If an enemy starts hacking your turret, it will become immobile and start to turn purple.
When it has been fully hacked by the enemy, it will try to attack you! Either destroy the turret or run up to it and
press fire (I) to hack it back onto your side. If you destroy a hacked turret you will get some money and points for it,
but if you destroy a turret on your team then you'll lose points.

ENEMIES
=======
You get money and points for destroying enemies.
Drones : These are the bog-standard spambots. They're pretty weak and pretty dumb, but there's lots of them!
Hackers : Hackers are basically fast drones, except that they can hack turrets!
Mech : These are great, hulking, axe wielding spambots. They have a large amount of health and can do a lot of damage to your sack of catnip.

FORFEIT
=======
You can forfeit the game from the pause menu. This will destroy your sack of catnip.

GAME OVER
=========
When your sack of catnip reaches zero health, you lose! Your score breakdown will be shown and you'll be asked to enter your name.
Use W and S to pick letters, and A and D to go backward and forward spaces. Press trigger (I) to confirm.

}

program main;
{$mode objfpc}{$H+}     { $define COMPILE_LEVEL_EDITOR}

uses SysUtils, Math, Display, ShaderClass, MusicClass, FontClass,
  GraphicalAssetClasses, InitialisationAndFreeing, ButtonClass, CatClass,
  SpambotClass, GlobalGameVariables
  {$ifdef COMPILE_LEVEL_EDITOR} , LevelEditor {$endif} ;

procedure mainMenu;
var
  logo : PSprite;
  tempCat : PCat;
  tempDrone : array[0..2] of PDrone;
  time, extraHeight : real;
  i : integer;
begin
  //load up the buttons, logo, cat, drones and music for the main menu
  initMainMenuButtons;
  resetMainMenu := false;

  logo := new(PSprite, create('logo', 'assets/sprites/logo.png', 0, 0, 500, 150));

  tempCat := new(PCat, create(0, 0));
  tempCat^.setPosition(20, 0, 0);
  tempCat^.rotate(0, -15, 0);
  tempCat^.setCurrentAnimation(2);

  for i := 0 to 2 do
  begin
    tempDrone[i] := new(PDrone, create(0, 0, true));
    tempDrone[i]^.setPosition(-30+(30*i), -28, 0);
    tempDrone[i]^.rotate(-15, 0, 0);
    tempDrone[i]^.scale(0.6, 0.6, 0.6);
    tempDrone[i]^.setCurrentAnimation(3);
  end;

  time := 0;
  xDistance := 0;
  yDistance := 30;
  zDistance := 0;
  rotation := 0;
  respawning := false;
  showHighScores := false;
  highScoreDisplayDelay := 0;

  music('menu')^.play;
  setup2dMatrices;
  repeat
    //draw a dark blue background by telling the font shader to set its colour tint
    //to dark blue, and then drawing a white rectangle, scaling to the size of the
    //screen
    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.2, 1.0);
    whiteScreen^.draw(0, 0, -1000, 0, skyXScale, skyYScale);

    //set up matrices and shaders for 3D drawing
    setup3dMatrices(true);
    tintShader^.bind;
    tintShader^.use;
    tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);

    //Increment the frame of the cat and drones so they dance to the exact tempo
    //of the music
    tempCat^.setFrame(1/1.125, true);
    for i := 0 to 2 do tempDrone[i]^.setFrame(1/1.125, true);

    //Make the cat and drones jump realistically by moving their y positions
    //according to a sine wave, except for when they hit the floor
    extraHeight := sin(degToRad((time/40)*360));
    if (extraHeight < 0) then
    begin
      extraHeight := 0;
      time := 0;
    end;
    tempCat^.setY(extraHeight*3);
    tempCat^.draw(false, true);
    tempCat^.drawShadow;

    for i := 0 to 2 do
    begin
      tempDrone[i]^.setY(-28+(extraHeight*2));
      tempDrone[i]^.draw(false, true);
    end;

    //Draw the outline for the cat and drones
    startOutline;
    tempCat^.draw(false, true);
    for i := 0 to 2 do tempDrone[i]^.draw(false, true);
    stopOutline;

    //Draw logo and highscores or menu buttons
    setup2dMatrices;
    logo^.draw(10, 10, -1);
    if (showHighScores) then
    begin
      for i := 0 to 9 do
      begin
        fontShader^.use;
        fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
        tenneryBold^.write(highScores[i].name, 30, 160+(20*i), -1, true, 0, 0.4, 0.4);
        tenneryBold^.write('-', 160, 160+(20*i), -1, true, 0, 0.4, 0.4);
        tenneryBold^.write(intToStr(highScores[i].score), 200, 160+(20*i), -1, true, 0, 0.4, 0.4);
      end;
      tenneryBold^.write('Press any button to return to the main menu', 30, 380, -1, true, 0, 0.5, 0.5);

      if (trigger and (highScoreDisplayDelay > 20)) then
      begin
        showHighScores := false;
        highScoreDisplayDelay := 0;
        clearFontCache;
      end else highScoreDisplayDelay += compensation;
    end
  else
    begin
      handleMenuInput;
      updateButtons;
    end;

    //If the player has just returned from the main menu from gameplay, reset the
    //neccessary settings
    if (resetMainMenu) then
    begin
      music('menu')^.play;
      time := 0;
      tempCat^.setPosition(20, 0, 0);
      tempCat^.rotate(0, -30, 0);
      tempCat^.setFrame(0);
      xDistance := 0;
      yDistance := 30;
      zDistance := 0;
      rotation := 0;
      respawning := false;
      resetMainMenu := false;
      setup2dMatrices;
      continue;
    end;

    //Refresh the screen and increment the timer
    refreshScreen;
    time += compensation/1.125;
  until quit;

  //Dispose of the cat, drones, logos and buttons loaded at the beginning of the
  //procedure
  dispose(tempCat, destroy);
  for i := 0 to 2 do dispose(tempDrone[i], destroy);
  dispose(logo, destroy);

  freeButtons;
end;

begin
  {Initialise everything, and then either go to the game (or level editor if
  COMPILE_LEVEL_EDITOR is defined. The highscores are read in before going to
  the main menu, which acts as the outermost game loop. Afterwards, free all the
  assets so the RAM and GPU memory is nice and clean!}

  initEverything;

  {$ifdef COMPILE_LEVEL_EDITOR}
  runLevelEditor;
  {$else}
  readHighScores;
  mainMenu;
  {$endif}

  freeEverything;
end.
