unit ButtonClass;

{$mode objfpc}{$H+}
{$define NO_BUTTON_LIMIT}

interface

uses SysUtils, GraphicalAssetClasses;

type
  PButton = ^TButton;
  TButton = object
  private
    hovering : boolean;
    selectionProcedure : procedure;
    hoverProcedure : procedure;
    text : string;
    x, y : integer;
    size: real;
  public
    constructor create(newText : string; destX, destY : integer; newSelectionProcedure : TProcedure;
                newHoverProcedure : TProcedure = nil; newSize : real = 1.0);
    destructor destroy;
    procedure hover;
    procedure select;
    procedure update;
  end;

procedure initPauseMenuButtons;
procedure initMainMenuButtons;
procedure freeButtons;

procedure pauseMenu;

procedure updateButtons;

procedure nextButton;
procedure previousButton;
procedure selectButton;

procedure handleMenuInput;

implementation

uses Classes, Math, SMSDL, Input, Display, Audio, FontClass, ShaderClass,
  MusicClass, GameActorClass, GlobalGameVariables, TurretClass, FloatScoreClass,
  ShopkeeperClass, SpambotClass, GunClass, InitialisationAndFreeing;

const
  SELECTION_DELAY = 30;
  MOVE_DELAY = 15;

  ITEM_SCALE = 0.2;
  ITEM_HIGHLIGHT_SCALE = 0.4;

var
  buttons, items, floatText : TList;
  buttonId : integer;
  goBack, breakUpdateLoop : boolean;
  timeFromSelect, timeFromMove, tempXDistance, tempYDistance, tempZDistance, tempRotation : real;
  destScale : array of real;
  shopkeeper : PShopkeeper;
  shopText : string = ' ';

procedure resumeGame;
begin
  paused := false;
  wait(200);
end;

procedure buyKittyTurret;
var
  tempTurret, tempGameActor : PGameActor;
  collision : boolean = true;
  i : integer;
begin
  if (money < KITTY_TURRET_PRICE) then exit;

  money -= KITTY_TURRET_PRICE;
  tempTurret := new(PKittyTurret, create(-tempXDistance+(sin(degToRad(-tempRotation+180))*40),
             -tempZDistance+(cos(degToRad(-tempRotation+180))*40), -tempRotation-180));
  gameActors.add(tempTurret);

  while (collision) do
  begin
    collision := false;
    for i := 0 to gameActors.count-1 do
    begin
      tempGameActor := PGameActor(gameActors[i]);
      if (tempGameActor = tempTurret) then continue;
      if (tempTurret^.collision(tempGameActor)) then
      begin
        collision := true;
        tempTurret^.setX(tempTurret^.x+(sin(degToRad(-rotation+180))*10));
        tempTurret^.setZ(tempTurret^.z+(cos(degToRad(-rotation+180))*10));
        break;
      end;
    end;
  end;

  shopkeeper^.talk(PURCHASE_SOUND);
  floatText.add(new(PFloatText, create('-$'+intToStr(KITTY_TURRET_PRICE), 14, 25, 0,
                                tenneryBold, 1.0, 0.0, 0.0)));
end;

procedure buyAkatTurret;
var
  tempTurret, tempGameActor : PGameActor;
  collision : boolean = true;
  i : integer;
begin
  if (money < AKAT_TURRET_PRICE) then exit;

  money -= AKAT_TURRET_PRICE;
  tempTurret := new(PAkatTurret, create(-tempXDistance+(sin(degToRad(-tempRotation+180))*40),
             -tempZDistance+(cos(degToRad(-tempRotation+180))*40), -tempRotation-180));
  gameActors.add(tempTurret);

  while (collision) do
  begin
    collision := false;
    for i := 0 to gameActors.count-1 do
    begin
      tempGameActor := PGameActor(gameActors[i]);
      if (tempGameActor = tempTurret) then continue;
      if (tempTurret^.collision(tempGameActor)) then
      begin
        collision := true;
        tempTurret^.setX(tempTurret^.x+(sin(degToRad(-rotation+180))*10));
        tempTurret^.setZ(tempTurret^.z+(cos(degToRad(-rotation+180))*10));
        break;
      end;
    end;
  end;

  shopkeeper^.talk(PURCHASE_SOUND);
  floatText.add(new(PFloatText, create('-$'+intToStr(AKAT_TURRET_PRICE), 14, 25, 0,
                                tenneryBold, 1.0, 0.0, 0.0)));
end;

procedure buyKittyGun;
begin
  if (money < KITTYGUN_PRICE) then exit;

  money -= KITTYGUN_PRICE;
  gameActors.add(new(PKittyGun, create(-tempXDistance, -tempZDistance)));

  shopkeeper^.talk(PURCHASE_SOUND);
  floatText.add(new(PFloatText, create('-$'+intToStr(KITTYGUN_PRICE), 14, 25, 0,
                                tenneryBold, 1.0, 0.0, 0.0)));
end;

procedure buyAkat;
begin
  if (money < AKAT_PRICE) then exit;

  money -= AKAT_PRICE;
  gameActors.add(new(PAkat, create(-tempXDistance, -tempZDistance)));

  shopkeeper^.talk(PURCHASE_SOUND);
  floatText.add(new(PFloatText, create('-$'+intToStr(AKAT_PRICE), 14, 25, 0,
                                tenneryBold, 1.0, 0.0, 0.0)));
end;

procedure buyBirdCatcher;
begin
  if (money < BIRDCATCHER_PRICE) then exit;

  money -= BIRDCATCHER_PRICE;
  gameActors.add(new(PBirdCatcher, create(-tempXDistance, -tempZDistance)));

  shopkeeper^.talk(PURCHASE_SOUND);
  floatText.add(new(PFloatText, create('-$'+intToStr(BIRDCATCHER_PRICE), 14, 25, 0,
                                tenneryBold, 1.0, 0.0, 0.0)));
end;

procedure buyAntiMech;
begin
  if (money < ANTIMECH_PRICE) then exit;

  money -= ANTIMECH_PRICE;
  gameActors.add(new(PAntiMech, create(-tempXDistance, -tempZDistance)));

  shopkeeper^.talk(PURCHASE_SOUND);
  floatText.add(new(PFloatText, create('-$'+intToStr(ANTIMECH_PRICE), 14, 25, 0,
                                tenneryBold, 1.0, 0.0, 0.0)));
end;

procedure highlightKittyTurret;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  shopText := 'Kitty Turret - $'+intToStr(KITTY_TURRET_PRICE);
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      if (tempGameActor^.name = TURRET_CHARACTER+'kittyTurret') then
         destScale[i] := ITEM_HIGHLIGHT_SCALE else destScale[i] := ITEM_SCALE;
    end;
  end;
end;

procedure highlightAkatTurret;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  shopText := 'A-KAT Turret - $'+intToStr(AKAT_TURRET_PRICE);
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      if (tempGameActor^.name = TURRET_CHARACTER+'akatTurret') then
         destScale[i] := ITEM_HIGHLIGHT_SCALE else destScale[i] := ITEM_SCALE;
    end;
  end;
end;

procedure highlightKittyGun;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  shopText := 'Kitty Gun - $'+intToStr(KITTYGUN_PRICE);
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      if (tempGameActor^.name = 'Kitty Gun') then
         destScale[i] := ITEM_HIGHLIGHT_SCALE else destScale[i] := ITEM_SCALE;
    end;
  end;
end;

procedure highlightAkat;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  shopText := 'A-KAT - $'+intToStr(AKAT_PRICE);
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      if (tempGameActor^.name = 'A-KAT') then
         destScale[i] := ITEM_HIGHLIGHT_SCALE else destScale[i] := ITEM_SCALE;
    end;
  end;
end;

procedure highlightBirdCatcher;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  shopText := 'Bird Catcher - $'+intToStr(BIRDCATCHER_PRICE);
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      if (tempGameActor^.name = 'Bird Catcher') then
         destScale[i] := ITEM_HIGHLIGHT_SCALE else destScale[i] := ITEM_SCALE;
    end;
  end;
end;

procedure highlightAntiMech;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  shopText := 'Anti-Mech - $'+intToStr(ANTIMECH_PRICE);
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      if (tempGameActor^.name = 'Anti-Mech') then
         destScale[i] := ITEM_HIGHLIGHT_SCALE else destScale[i] := ITEM_SCALE;
    end;
  end;
end;

procedure back;
begin
  goBack := true;
end;

procedure unhighlightItems;
var
  i : integer;
begin
  shoptext := ' ';
  if (items.count > 0) then
  begin
    for i := 0 to items.count-1 do destScale[i] := ITEM_SCALE;
  end;
end;

procedure initShopButtons;
var
  tempButton : PButton;
begin
  buttons.add(new(PButton, create('BACK', 20, 20, @back, @unhighlightItems)));
  buttons.add(new(PButton, create(' ', 380, 280, @buyAkatTurret, @highlightAkatTurret)));
  buttons.add(new(PButton, create(' ', 280, 280, @buyKittyTurret, @highlightKittyTurret)));
  buttons.add(new(PButton, create(' ', 180, 280, @buyKittyGun, @highlightKittyGun)));
  buttons.add(new(PButton, create(' ', 155, 300, @buyAkat, @highlightAkat)));
  buttons.add(new(PButton, create(' ', 115, 320, @buyBirdCatcher, @highlightBirdCatcher)));
  buttons.add(new(PButton, create(' ', 60, 360, @buyAntiMech, @highlightAntiMech)));
  buttonId := 0;
  tempButton := PButton(buttons[0]);
  tempButton^.hover;
  timeFromSelect := 0;
  timeFromMove := 0;
  showCursor;
end;

procedure shop;
var
  tempGameActor, dunceDrone : PGameActor;
  props : TList;
  time : real;
  i : integer;
  greeted : boolean = false;
begin
  freeButtons;
  goBack := false;

  shopkeeper := new(PShopkeeper, create(30, -8));
  shopkeeper^.rotate(0, -20, 0);
  props := TList.create;
  props.add(new(PProp, create(TABLE, 5, -0.5, 0)));
  props.add(new(PProp, create(TABLE, -20, -0.5, 13)));
  tempGameActor := PGameActor(props.last);
  tempGameActor^.rotate(0, 90, 0);

  props.add(new(PProp, create(TILL, 15, 10, 0)));
  tempGameActor := PGameActor(props.last);
  tempGameActor^.rotate(0, 130, 0);

  props.add(new(PProp, create(HAT, -60, 18.5, -80)));
  tempGameActor := PGameActor(props.last);
  tempGameActor^.rotate(0, -60, 0);

  items.add(new(PKittyTurret, create(-10, 0, 20)));
  items.add(new(PAkatTurret, create(0, 0, 20)));
  items.add(new(PKittyGun, create(-19, 4)));
  items.add(new(PAkat, create(-19, 13)));
  items.add(new(PBirdCatcher, create(-19, 22)));
  items.add(new(PAntiMech, create(-19, 31)));
  setLength(destScale, items.count);
  for i := 0 to items.count-1 do
  begin
    tempGameActor := PGameActor(items[i]);
    tempGameActor^.setY(10);
    tempGameActor^.scale(ITEM_SCALE, ITEM_SCALE, ITEM_SCALE);
    destScale[i] := ITEM_SCALE;
  end;

  dunceDrone := new(PDrone, create(-60, -80));
  dunceDrone^.setY(-5);
  dunceDrone^.rotate(0, 30, 0);
  dunceDrone^.setCurrentAnimation(2);


  tempXDistance := xDistance;
  tempYDistance := yDistance;
  tempZDistance := zDistance;
  tempRotation := rotation;

  time := 0;
  shopkeeper^.setCurrentAnimation(1);
  shopkeeper^.talk(WELCOME_SOUND);

  initShopButtons;

  setup2dMatrices;

  repeat
    if (not goBack) then handleMenuInput;

    xDistance := 0;
    yDistance := 25;
    zDistance := 0;
    rotation := 0;

    sky^.draw(0, 0, -1000, 0, skyXScale, skyYScale);

    setup3dMatrices(true);

    drawFloor;

    if (goBack) then
    begin
      if (greeted) then
      begin
        shopkeeper^.talk(GOODBYE_SOUND);
        greeted := false;
      end else time += compensation;
    end
  else
    begin
      if (not greeted) then
      begin
        if (time >= 120) then
        begin
          shopkeeper^.setCurrentAnimation(0);
          greeted := true;
          time := 0;
        end else time += compensation;
      end;
    end;

    shopkeeper^.update;
    tintShader^.bind;
    dunceDrone^.draw(false, true);
    dunceDrone^.drawShadow;

    if (floatText.count > 0) then
    begin
      for i := 0 to  floatText.count-1 do
      begin
        tempGameActor := PGameActor(floatText[i]);
        tempGameActor^.update;
      end;
    end;

    for i := 0 to props.count-1 do
    begin
      tempGameActor := PGameActor(props[i]);
      tempGameActor^.update;
    end;

    tintShader^.bind;
    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);

      if ((tempGameActor^.xScale > destScale[i]-compensation*0.2) and
         (tempGameActor^.xScale < destScale[i]+compensation*0.2)) then
         tempGameActor^.scale(destScale[i], destScale[i], destScale[i]) else
      begin
        if (tempGameActor^.xScale > destScale[i]) then
          tempGameActor^.scale(-compensation*0.1, -compensation*0.1, -compensation*0.1, true)
          else tempGameActor^.scale(compensation*0.1, compensation*0.1, compensation*0.1, true);
      end;

      if (tempGameActor^.xScale > ITEM_HIGHLIGHT_SCALE) then tempGameActor^.rotate(0, 1, 0, true)
         else tempGameActor^.rotate(0, 20, 0);
      if (tempGameActor^.name[1] <> TURRET_CHARACTER) then
        tempGameActor^.scale(tempGameActor^.xScale*2, tempGameActor^.yScale*2, tempGameActor^.zScale*2);
      tempGameActor^.draw(true, true);
    end;


    startOutline;
    shopkeeper^.draw(false, true);
    dunceDrone^.draw(false, true);

    for i := 0 to props.count-1 do
    begin
      tempGameActor := PGameActor(props[i]);
      tempGameActor^.draw(true, true);
    end;

    for i := 0 to items.count-1 do
    begin
      tempGameActor := PGameActor(items[i]);
      tempGameActor^.draw(true, true);
      if (tempGameActor^.name[1] <> TURRET_CHARACTER) then
        tempGameActor^.scale(tempGameActor^.xScale*0.5, tempGameActor^.yScale*0.5,
                                                        tempGameActor^.zScale*0.5);
    end;
    stopOutline;

    disableDepthTesting;
    drawText;
    enableDepthTesting;

    setup2dMatrices;
    {$ifdef NO_BUTTON_LIMIT}
    unhighlightItems;
    {$endif}
    updateButtons;

    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.0, 1.0);
    tenneryBold^.write(shopText, round((screenWidth*0.5)-(tenneryBold^.width(shopText)*0.5)),
                                 screenHeight-60, -1);
    tenneryBold^.write('You have $'+intToStr(money),
      screenWidth-20-tenneryBold^.width('You have $'+intToStr(money)), 20, -1);

    refreshScreen;
  until (goBack and (time >= 80));

  xDistance := tempXDistance;
  yDistance := tempYDistance;
  zDistance := tempZDistance;
  rotation := tempRotation;

  dispose(shopkeeper, destroy);
  dispose(dunceDrone, destroy);

  for i := 0 to floatText.count-1 do
  begin
    tempGameActor := PGameActor(floatText[i]);
    dispose(tempGameActor, destroy);
  end;
  floatText.clear;

  for i := 0 to props.count-1 do
  begin
    tempGameActor := PGameActor(props[i]);
    dispose(tempGameActor, destroy);
  end;
  props.destroy;

  for i := 0 to items.count-1 do
  begin
    tempGameActor := PGameActor(items[i]);
    dispose(tempGameActor, destroy);
  end;
  items.clear;

  goBack := false;
  freeButtons;
  initPauseMenuButtons;
  breakUpdateLoop := true;
end;

procedure forfeit;
begin
  sack^.doDamage(SACK_HEALTH);
  resumeGame;
end;

procedure quitToMainMenu;
begin
  paused := false;
  quit := true;
  wait(200);
end;

procedure initPauseMenuButtons;
var
  tempButton : PButton;
begin
  buttons.add(new(PButton, create('RESUME', 20, 60, @resumeGame)));
  buttons.add(new(PButton, create('SHOP', 20, 120, @shop)));
  buttons.add(new(PButton, create('FORFEIT', 20, 180, @forfeit)));
  buttons.add(new(PButton, create('QUIT TO MAIN MENU', 20, 240, @quitToMainMenu)));
  buttons.add(new(PButton, create('QUIT TO DESKTOP', 20, 300, @quitGame)));
  buttonId := 0;
  tempButton := PButton(buttons[0]);
  tempButton^.hover;
  timeFromSelect := 0;
  timeFromMove := 0;
  showCursor;
end;

procedure pauseMenu;
begin
  paused := true;
  musicVolume(10);
  euphoriaMusic^.setVolume(10);

  initPauseMenuButtons;

  repeat
    drawStill;
    drawSprites;
    drawText;

    setup2dMatrices;

    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.0, 0.6);
    whiteScreen^.draw(0, 0, -1, 0, skyXScale, skyYScale);

    handleMenuInput;
    updateButtons;

    refreshScreen;
  until not paused;

  freeButtons;
  musicVolume(50);
  if (euphoriaMusic^.playing) then euphoriaMusic^.setVolume(50);
end;

procedure playMap1;
begin
  freeButtons;
  loadMap('map1');
  game;
  freeMap;
  initMainMenuButtons;
  resetMainMenu := true;
end;

procedure playMap2;
begin
  freeButtons;
  loadMap('map2');
  game;
  freeMap;
  initMainMenuButtons;
  resetMainMenu := true;
end;

procedure highScores;
begin
  showHighScores := true;
  highScoreDisplayDelay := 0;
end;

procedure options;
begin

end;

procedure initMainMenuButtons;
var
  tempButton : PButton;
begin
  buttons.add(new(PButton, create('PLAY MAP 1', 20, 180, @playMap1)));
  buttons.add(new(PButton, create('PLAY MAP 2', 20, 240, @playMap2)));
  buttons.add(new(PButton, create('HIGH SCORES', 20, 300, @highScores)));
  //buttons.add(new(PButton, create('OPTIONS', 20, 300, @options)));
  buttons.add(new(PButton, create('QUIT', 20, 360, @quitGame)));
  buttonId := 0;
  tempButton := PButton(buttons[0]);
  tempButton^.hover;
  timeFromSelect := 0;
  timeFromMove := 0;
  showCursor;
end;

procedure freeButtons;
var
  i : integer;
  tempButton : PButton;
begin
  if (buttons.count > 0) then
  begin
    for i := 0 to buttons.count-1 do
    begin
      tempButton := PButton(buttons[i]);
      dispose(tempButton, destroy);
    end;
  end;
  buttons.clear;
  hideCursor;
end;

procedure updateButtons;
var
  i : integer;
  tempButton : PButton;
begin
  breakUpdateLoop := false;
  for i := 0 to buttons.count-1 do
  begin
    tempButton := PButton(buttons[i]);
    tempButton^.update;
    if (breakUpdateLoop) then
    begin
      breakUpdateLoop := false;
      break;
    end;
  end;
end;

procedure nextButton;
var
  tempButton : PButton;
begin
  buttonId += 1;
  if (buttonId >= buttons.count) then buttonId := 0;
  tempButton := PButton(buttons[buttonId]);
  tempButton^.hover;
end;

procedure previousButton;
var
  tempButton : PButton;
begin
  buttonId -= 1;
  if (buttonId < 0) then buttonId := buttons.count-1;
  tempButton := PButton(buttons[buttonId]);
  tempButton^.hover;
end;

procedure selectButton;
var
  tempButton : PButton;
begin
  if (buttonId >= buttons.count) then buttonId := 0
     else if (buttonId < 0) then buttonId := buttons.count-1;
  if (buttons.count > 0) then
  begin
    tempButton := PButton(buttons[buttonId]);
    tempButton^.select;
  end;
end;

procedure handleMenuInput;
begin
  if (timeFromMove >= MOVE_DELAY) then
  begin
    if (up) then
    begin
      previousButton;
      timeFromMove := 0;
    end;
    if (down) then
    begin
      nextButton;
      timeFromMove := 0;
    end;
  end else timeFromMove += compensation;
  if (timeFromSelect >= SELECTION_DELAY) then
  begin
    if (trigger) then
    begin
      {$ifdef NO_BUTTON_LIMIT}
      if (mouseLeft) then exit;
      {$endif}
      selectButton;
      timeFromSelect := 0;
    end;
  end else timeFromSelect += compensation;
end;

constructor TButton.create(newText : string; destX, destY : integer; newSelectionProcedure : TProcedure;
            newHoverProcedure : TProcedure = nil; newSize : real = 1.0);
begin
  text := newText;
  x := destX;
  y := destY;
  hovering := false;
  selectionProcedure := newSelectionProcedure;
  hoverProcedure := newHoverProcedure;
  size := newSize;
  tenneryBold^.cache(text);
end;

destructor TButton.destroy;
begin
  tenneryBold^.removeFromCache(text);
end;

procedure TButton.hover;
var
  i : integer;
  tempButton : PButton;
begin
  for i := 0 to buttons.count-1 do
  begin
    tempButton := PButton(buttons[i]);
    tempButton^.hovering := false;
  end;
  hovering := true;
  if (hoverProcedure <> nil) then hoverProcedure;
end;

procedure TButton.select;
begin
  selectionProcedure;
end;

procedure TButton.update;
begin
  {$ifdef NO_BUTTON_LIMIT}
  if ((mouseX > x) and (mouseX < x+tenneryBold^.width(text))) then
  begin
    if ((mouseY > y) and (mouseY < y+tenneryBold^.height(text))) then
    begin
      hover;
      buttonId := buttons.indexOf(@self);
      if (mouseLeft) then select;
    end else hovering := false;
  end else hovering := false;
  {$endif}
  fontShader^.use;
  if (hovering) then fontShader^.setUniform4(EXTRA0_LOCATION, 0.7, 0.7, 0.7, 1.0)
     else fontShader^.setUniform4(EXTRA0_LOCATION, 0.9, 0.9, 0.9, 1.0);
  if (text = '') then text := ' ';
  tenneryBold^.write(text, x, y, -1, true, 0.0, size, size);
end;

initialization

buttons := TList.create;
items := TList.create;
floatText := TList.create;

finalization

buttons.destroy;
items.destroy;
floatText.destroy;

end.

