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
