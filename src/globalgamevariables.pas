unit GlobalGameVariables;

{$mode objfpc}{$H+}
{$define NO_BUTTON_LIMIT}
interface

uses Classes, ShaderClass, TextureClass, GraphicalAssetClasses, FontClass, SoundClass, GameActorClass, CatClass, SackClass, SightClass, HudClass, SpambotClass;

const
  {$ifndef TOP_DOWN}
  {$ifndef FIRST_PERSON}
  VIEW_X_DIST = 0;
  VIEW_Y_DIST = -35;
  VIEW_Z_DIST = -80;
  VIEW_ANGLE = 10;
  {$endif}
  {$endif}

  {$ifdef TOP_DOWN}
  VIEW_X_DIST = 0;
  VIEW_Y_DIST = 0;
  VIEW_Z_DIST = -300;
  VIEW_ANGLE = 90;
  {$endif}

  {$ifdef FIRST_PERSON}
  VIEW_X_DIST = 0;
  VIEW_Y_DIST = -17;
  VIEW_Z_DIST = 6;
  VIEW_ANGLE = 0;
  {$endif}

  DEFAULT_VIEW_X_DIST = 0;
  DEFAULT_VIEW_Y_DIST = -35;
  DEFAULT_VIEW_Z_DIST = -80;
  DEFAULT_VIEW_ANGLE = 10;

  CAT_MOVEMENT_SPEED = 1.5;
  CAT_ROTATION_SPEED = 2;
  CAT_ANIMATION_SPEED = 2;

  X_LOWER_BOUND = -200;
  X_UPPER_BOUND = 1800;
  Z_LOWER_BOUND = -200;
  Z_UPPER_BOUND = 1800;

  RAY_PRECISION = 10;
  RAY_MAX_LENGTH = 675;

  TURRET_ROTATION_SPEED = 0.5;
  TURRET_RANGE = 300;
  TURRET_HEALTH = 100;
  TURRET_MAX_LEVEL = 3;

  TURRET_HACK_TIME = 300;
  TURRET_HACK_TEXT = 'Press fire to hack';
  TURRET_HACKING_TEXT = 'Hacking';

  TURRET_HEALTH_REPLENISH_AMOUNT = 100;

  KITTY_TURRET_PRICE = 120;
  AKAT_TURRET_PRICE = 200;
  FIRE_UPGRADE_PRICE = 100;
  RANGE_UPGRADE_PRICE = 100;
  HEALTH_UPGRADE_PRICE = 50;
  HEALTH_REPLENISH_PRICE = 50;
  TURRET_UPGRADE_TEXT = 'Press fire to buy an upgrade!';
  TURRET_HEAL_TEXT = 'Press fire to buy repairs!';

  KITTYGUN_PRICE = 100;
  AKAT_PRICE = 200;
  BIRDCATCHER_PRICE = 200;
  ANTIMECH_PRICE = 300;

  ENEMY_TURRET = 0;
  FRIENDLY_TURRET = 1;

  MAXIMUM_SACK_DISTANCE_FOR_ATTACK_SQUARED = 10000;

  MAXIMUM_DRAW_ANGLE = 50;

  PLAYER_CHARACTER = '!';
  ENEMY_CHARACTER = '@';
  SCENERY_CHARACTER = '#';
  TURRET_CHARACTER = '>';

  DAMAGE_TINT_TIME = 2;

  PLAYER_DEFAULT_RESPAWN_X = 0;
  PLAYER_DEFAULT_RESPAWN_Z = 0;
  PLAYER_RESPAWN_ROTATION = 135;
  PLAYER_RESPAWN_TIME = 300;

  RESPAWN_VIEW_X_DIST = 100;
  RESPAWN_VIEW_Y_DIST = -100;
  RESPAWN_VIEW_Z_DIST = -400;
  RESPAWN_VIEW_ANGLE = 30;

  A_LOT_OF_MONEY = 10000;

  SACK_HEALTH = 100000;

  SPAMBOT_REWARD : array[DRONE..MECH] of integer = (10, 10, 50);
  TURRET_REWARD = 30;

  MAX_HEARING_DISTANCE_SQAURED = 100000;

  MECH_MINIMUM_SPAWN_SCORE = 1000;
  HACKER_MINIMUM_SPAWN_SCORE = 500;

  //configuration detail constants
  CONFIG_SCREEN_WIDTH = 0;
  CONFIG_SCREEN_HEIGHT = 1;
  CONFIG_FULLSCREEN = 2;
  CONFIG_USING_CONTROLLER = 3;

type
  highScoreRecord = record
    name : string[8];
    score : integer;
  end;

var
  sky, shadowCircle, shadowDot, smoke, whiteScreen : PSprite;
  lightningSprite : array[0..6] of PSprite;
  floorModel, catModel, catWithSpannerModel : PModel;
  floorObject : PGameObject;
  spriteShader, shadowShader, modelShader, flatShader, lineShader, skeletonLineShader, fontShader, tintShader, dissolveShader, drainVerticalShader, drainHorizontalShader, tintAndWearShader,
    alternateTextureShader : PShader;
  tenneryBold, pictosWeb : PFont;
  rotation, xDistance, yDistance, zDistance, skyXScale, skyYScale, euphoriaBuildup, turretHackTextWidthOverTwo, turretUpgradeTextWidthOverTwo, turretHealTextWidthOverTwo, turretHackingTextWidthOverTwo,
    soundEffectsVolume, highScoreDisplayDelay : real;
  gameActors, collidables, spambots, turrets, spawnPoints, catSpawnPoints : TList;
  cat : PCat = nil;
  sack : PSack = nil;
  sight : PSight = nil;
  hud : PHud = nil;
  money, score, euphoriaBonus, timeLeftBeforeRespawn, droneKills, hackerKills, mechKills : integer;
  paused, quit, respawning, catJustDamaged, usingController, resetMainMenu, showHighScores : boolean;
  dissolveMap, euphoriaTexture : PTexture;
  explosionSound, euphoriaSequenceSound, euphoriaMusic, bipSound, cashRegisterSound, kittyGunSound : PSound;
  spambotKills : array[DRONE..MECH] of integer;
  turretKills : array[ENEMY_TURRET..FRIENDLY_TURRET] of integer;
  configDetails : array[CONFIG_SCREEN_WIDTH..CONFIG_USING_CONTROLLER] of integer;
  highScores : array[0..9] of highScoreRecord;
  {$ifdef NO_BUTTON_LIMIT}
  mouseMoveAmount : integer = 0;
  {$endif}

procedure readHighScores;
procedure game;

procedure setup3dMatrices(useDefault : boolean = false);
procedure setup2dMatrices;

procedure drawFloor;

procedure drawShadow(pClass : Pointer; shader : PShader; data : Pointer);

function up : boolean;
function down : boolean;
function left : boolean;
function right : boolean;
function clockwise : boolean;
function anticlockwise : boolean;
function trigger : boolean;
function pause : boolean;

procedure startOutline;
procedure stopOutline;

procedure initLines;
procedure freeLines;
procedure drawLine(x, y, z, length, thickness, rotation, r, g, b, a : real);

//Need to buffer 3D text and sprites so that they can be drawn after models, so that alpha blending works correctly
procedure bufferText(text : string; x, y, z, originX, scale, r, g, b, a : real; fontToUse : PFont);
procedure drawText;
procedure bufferSprite(sprite : PSprite; x, y, z, scale, r, g, b, a : real);
procedure drawSprites;

function hitScan(x, z, rotation, maxLength : real; var actor : PGameActor; onlyEnemies : boolean = false; ignoreCat : boolean = false) : real;

procedure setRespawnTimer;
procedure respawn;

procedure bindDissolveMap;
procedure bindAlternateTexture(texture : PTexture);

procedure drawStill;

procedure randomDrop(x, z : real);

implementation

uses SysUtils, Math, dglOpenGL, Input, Display, Audio, MusicClass, SpawnPointClass, PickupClass, GunClass, ButtonClass, ExplosionClass, InitialisationAndFreeing;

type
  textBufferRecord = record
    text : string;
    x, y, z, originX, scale, r, g, b, a : real;
    fontToUse : PFont;
  end;

  spriteBufferRecord = record
    sprite : PSprite;
    x, y, z, scale, r, g, b, a : real;
  end;

  PRespawnTimer = ^TRespawnTimer;
  TRespawnTimer = object(TGameActor)
  private
    time : real;
    timeInSeconds : integer;
  public
    constructor create;
    destructor destroy; virtual;

    procedure update; virtual;
  end;

var
  lineVao, lineVbo : GLuint;
  textBuffer : array[0..511] of textBufferRecord;
  spriteBuffer : array[0..511] of spriteBufferRecord;
  textBufferCount, spriteBufferCount : integer;

constructor TRespawnTimer.create;
begin
  TGameObject.create('respawnTimer', 0, 0, 0, PModel(nil));
  dieFlag := false;
  time := PLAYER_RESPAWN_TIME;
  timeInSeconds := round(PLAYER_RESPAWN_TIME/60);
  timeLeftBeforeRespawn := timeInSeconds;
  respawning := true;
  xDistance := -sack^.x;
  zDistance := -sack^.z;
  rotation := 30;
end;

destructor TRespawnTimer.destroy;
begin
  TGameObject.destroy;
end;

procedure TRespawnTimer.update;
begin
  time -= compensation;
  if (time < (timeInSeconds-1)*60) then timeInSeconds -= 1;
  timeLeftBeforeRespawn := timeInSeconds;
  if (timeInSeconds <= 0) then
  begin
    respawn;
    dieFlag := true;
  end;
end;

procedure handleGameInput;
var
  newXDistance, newZDistance : real;
  i, id : integer;
  collision : boolean = false;
  tempGameActor : PGameActor;
begin
  if (cat <> nil) then
  begin
    newXDistance := xDistance;
    newZDistance := zDistance;
    {$ifdef NO_BUTTON_LIMIT}
    if (anticlockwise) then rotation -= compensation*mouseMoveAmount*0.2;
    if (clockwise) then rotation += compensation*mouseMoveAmount*0.2;
    {$else}
    if (anticlockwise) then rotation -= compensation*CAT_ROTATION_SPEED;
    if (clockwise) then rotation += compensation*CAT_ROTATION_SPEED;
    {$endif}
    if (rotation < 0) then rotation += 360 else if (rotation >= 360) then rotation -= 360;
    if (up) then
    begin
      newZDistance += cos(degToRad(rotation))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
      newXDistance -= sin(degToRad(rotation))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
    end;
    if (down) then
    begin
      newZDistance -= cos(degToRad(rotation))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
      newXDistance += sin(degToRad(rotation))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
    end;
    if (left) then
    begin
      newZDistance -= cos(degToRad(rotation+90))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
      newXDistance += sin(degToRad(rotation+90))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
    end;
    if (right) then
    begin
      newZDistance += cos(degToRad(rotation+90))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
      newXDistance -= sin(degToRad(rotation+90))*compensation*CAT_MOVEMENT_SPEED*euphoriaBonus;
    end;
    cat^.setX(-newXDistance);
    cat^.setZ(-newZDistance);

    if (collidables.count > 0) then
    begin
      for i := 0 to collidables.count-1 do
      begin
        tempGameActor := PGameActor(collidables[i]);
        if (tempGameActor <> PGameActor(cat)) then
        begin
          if (cat^.collision(tempGameActor)) then
          begin
            collision := true;
            id := i;
            break;
          end;
        end;
      end;
    end;

    if (not collision) then
    begin
      if ((-newXDistance > X_LOWER_BOUND) and (-newXDistance < X_UPPER_BOUND)) then xDistance := newXDistance;
      if ((-newZDistance > Z_LOWER_BOUND) and (-newZDistance < Z_UPPER_BOUND)) then zDistance := newZDistance;
    end
  else
    begin
      tempGameActor := PGameActor(collidables[id]);
      if (not (up or down or left or right)) then cat^.pushed := true;
      {if (tempGameActor^.isMoveable) then
      begin
        xDistance -= sin(degToRad(tempGameActor^.yRotation))*compensation*tempGameActor^.getSpeed;
        zDistance -= cos(degToRad(tempGameActor^.yRotation))*compensation*tempGameActor^.getSpeed;
      end;}
    end;
  end;
end;

procedure updateGameActors;
var
  i : word;
  tempGameActor : PGameActor;
begin
  tintShader^.bind;
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
  i := 0;
  while (i < gameActors.count) do
  begin
    tempGameActor := PGameActor(gameActors[i]);

    if (tempGameActor^.dieFlag) then
    begin
      dispose(tempGameActor, destroy);
      gameActors.delete(i);
      continue;
    end;

    tempGameActor^.updateVisibility;
    tempGameActor^.update;

    if (tempGameActor^.dieFlag) then
    begin
      dispose(tempGameActor, destroy);
      gameActors.delete(i);
      continue;
    end;
    i += 1;
  end;

  //render spawn points last so that the alpha blending works correctly
  i := 0;
  while (i < spawnPoints.count) do
  begin
    tempGameActor := PGameActor(spawnPoints[i]);
    if (tempGameActor^.dieFlag) then
    begin
      dispose(tempGameActor, destroy);
      gameActors.delete(i);
      continue;
    end;

    tempGameActor^.updateVisibility;
    tempGameActor^.update;
    i += 1;
  end;

  startOutline;
  if (gameActors.count > 0) then
  begin
    for i := 0 to gameActors.count-1 do
    begin
      tempGameActor := PGameActor(gameActors[i]);
      if (cat = nil) then
      begin
        if (tempGameActor^.name[1] = PLAYER_CHARACTER) then continue;
      end;
      if (tempGameActor^.visible) then tempGameActor^.draw(false, true);
    end;
  end;

  if (spawnPoints.count > 0) then
  begin
    for i := 0 to spawnPoints.count-1 do
    begin
      tempGameActor := PGameActor(spawnPoints[i]);
      if (tempGameActor^.visible) then tempGameActor^.draw(true, true);
    end;
  end;
  stopOutline;

  drawSprites;
  drawText;
end;

procedure euphoriaSequence;
var
  tempRot, time, intensity : real;
  i : integer;
begin
  tempRot := 0;

  repeat
    if (yDistance > 15) then yDistance := 15 else yDistance += compensation;

    rotation -= tempRot;

    if (tempRot > 180-compensation*8) then tempRot := 180 else tempRot += compensation*8;

    rotation += tempRot;

    drawStill;
    setup2dMatrices;
    refreshScreen;
  until ((yDistance = 15) and (tempRot = 180));

  time := 0;
  cat^.setCurrentAnimation(1);
  euphoriaSequenceSound^.play(round(100*soundEffectsVolume));
  repeat
    cat^.setFrame(2, true);
    time += compensation;

    drawStill;

    fontShader^.bind;
    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
    disableDepthTesting;
    pushMatrix;
      translateMatrix(cat^.x, 10, cat^.z);
      rotateMatrix(-rotation+180, 0, 1, 0);
      scaleMatrix(0.5, 0.5, 1.0);
      for i := 0 to 15 do
      begin
        if (i > 0) then rotateMatrix(22.5, 0, 0, i*1);
        lightningSprite[random(7)]^.draw(0, 0, 0);
      end;
    popMatrix;
    enableDepthTesting;

    bindAlternateTexture(euphoriaTexture);
    alternateTextureShader^.bind;
    alternateTextureShader^.use;
    alternateTextureShader^.setUniform1(EXTRA1_LOCATION, 1);
    alternateTextureShader^.setUniform1(EXTRA2_LOCATION, 1);

    intensity := 1.0+(120/time);
    alternateTextureShader^.setUniform4(EXTRA3_LOCATION, intensity, intensity, 1.0, 1.0);
    cat^.draw(false, true);
    startOutline;
    cat^.draw(false, true);
    stopOutline;

    tintShader^.bind;

    setup2dMatrices;
    refreshScreen;
  until time > 120;

  cat^.setCurrentAnimation(0);

  repeat
    if (yDistance < 0) then yDistance := 0 else yDistance -= compensation;

    rotation -= tempRot;

    if (tempRot < compensation*8) then tempRot := 0 else tempRot -= compensation*8;

    rotation += tempRot;

    drawStill;

    bindAlternateTexture(euphoriaTexture);
    alternateTextureShader^.bind;
    alternateTextureShader^.use;
    alternateTextureShader^.setUniform1(EXTRA1_LOCATION, 1);
    alternateTextureShader^.setUniform1(EXTRA2_LOCATION, 1);
    cat^.draw(false, true);
    startOutline;
    cat^.draw(false, true);
    stopOutline;
    tintShader^.bind;

    setup2dMatrices;
    refreshScreen;
  until ((yDistance = 0) and (tempRot = 0));
end;

procedure manageEuphoria;
begin
  if (euphoriaBuildup >= 100) then
  begin
    euphoriaBonus := 2;
    euphoriaBuildup := 99.9;
    pauseMusic;
    euphoriaSequence;
    euphoriaMusic^.play(50, 15, -1);
  end else if (euphoriaBonus > 1) then
  begin
    euphoriaBuildup -= 0.1*compensation;
    if (euphoriaBuildup <= 0) then
    begin
      euphoriaBuildup := 0;
      euphoriaBonus := 1;
      euphoriaMusic^.fade(2000);
      resumeMusic;
    end;
  end;
end;

procedure readHighScores;
var
  highScoreFile : text;
  i : integer;
  tempStr : string;
begin
  for i := 0 to 9 do
  begin
    highScores[i].name := 'EMPTY';
    highScores[i].score := 0;
  end;

  i := 0;
  if (directoryExists('assets/savefiles')) then
  begin
    if (fileExists('assets/savefiles/highscores.txt')) then
    begin
      assign(highScoreFile, 'assets/savefiles/highscores.txt');
      reset(highScoreFile);
      repeat
        if (i > 9) then break;
        readln(highScoreFile, tempStr);
        highScores[i].name := tempStr;

        readln(highScoreFile, tempStr);
        highScores[i].score := strToInt(tempStr);
        i += 1;
      until eof(highScoreFile);
      close(highScoreFile);
    end;
  end;
end;

procedure writeHighScores;
var
  highScoreFile : text;
  i : integer;
begin
  if (directoryExists('assets/savefiles')) then
  begin
    assign(highScoreFile, 'assets/savefiles/highscores.txt');
    rewrite(highScoreFile);
    for i := 0 to 9 do
    begin
      writeln(highScoreFile, highScores[i].name);
      writeln(highScoreFile, highScores[i].score);
    end;
    close(highScoreFile);
  end
else
  begin
    mkdir('assets/savefiles');
    writeHighScores;
  end;
end;

procedure addHighScore(newName : string; newScore : integer);
var
  i : integer;
  tempHighScores : array[0..9] of highScoreRecord;
  added : boolean = false;
begin
  if (length(newName) > 8) then setLength(newName, 8);

  for i := 0 to 9 do
  begin
    if (newScore > highScores[i].score) then
    begin
      tempHighScores[i].name := newName;
      tempHighScores[i].score := newScore;
      added := true;
      break;
    end else tempHighScores[i] := highScores[i];
  end;

  if (added and (i < 9)) then for i := i+1 to 9 do tempHighScores[i] := highScores[i-1];

  highScores := tempHighScores;
end;

procedure loseSequence;
type
  PList = ^TList;
  gameOverChar = record
    x, y : real;
    character : char;
  end;
  breakdownRecord = record
    text : string;
    x, y, time : integer;
    soundPlayed : boolean;
  end;

  procedure setup3dMatrices(x, y, z, xRot, yRot : real);
  begin
    setMatrix(MODELVIEW_MATRIX);
    copyMatrix(PERSPECTIVE_MATRIX, PROJECTION_MATRIX);
    copyMatrix(IDENTITY_MATRIX, MODELVIEW_MATRIX);
    rotateMatrix(xRot, 1, 0, 0);
    rotateMatrix(yRot, 0, 1, 0);
    translateMatrix(x, y, z);
  end;

  procedure drawStill(x, y, z, xRot, yRot : real);
  var
    i : integer;
    tempGameActor : PGameActor;
  begin
    setup2dMatrices;
    sky^.draw(0, 0, -1000, 0, skyXScale, skyYScale);
    setup3dMatrices(x, y, z, xRot, yRot);
    drawFloor;
    if (gameActors.count > 0) then
    begin
      tintShader^.bind;
      tintShader^.use;
      tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
      for i := 0 to gameActors.count-1 do
      begin
        tempGameActor := PGameActor(gameActors[i]);
        if (tempGameActor^.visible) then
        begin
          tempGameActor^.draw(false, true);
          tempGameActor^.drawShadow;
        end;
      end;

      startOutline;
      for i := 0 to gameActors.count-1 do
      begin
        tempGameActor := PGameActor(gameActors[i]);
        if (cat = nil) then
        begin
          if (tempGameActor^.name[1] = PLAYER_CHARACTER) then continue;
        end;
        if (tempGameActor^.visible) then tempGameActor^.draw(false, true);
      end;
      stopOutline;
    end;
  end;

  function rotationToSack(x, z : real) : real;
  var
    xDiff, zDiff, rot : real;
  begin
    xDiff := x-sack^.x;
    if (xDiff = 0) then xDiff := 1;
    zDiff := z-sack^.z;
    rot := radToDeg(arcTan(zDiff/xDiff));
    if (rot < 0) then rot := -rot-90 else rot := -rot+90;
    if (zDiff > 0) then rot := rot+180;
    result := rot;
  end;

  procedure updateExplosions(explosions : PList);
  var
    i : integer;
    tempExplosion : PGameActor;
  begin
    i := 0;
    while (i < explosions^.count) do
    begin
      tempExplosion := PGameActor(explosions^[i]);
      tempExplosion^.updateVisibility;
      tempExplosion^.update;
      if (tempExplosion^.dieFlag) then explosions^.delete(i) else i += 1;
    end;
    drawSprites;
  end;

const
  GAME_OVER_TEXT = 'GAME OVER';
var
  viewXRot, time, timeSinceLastExplosion, textHeight, waitTime : real;
  explosions : TList;
  i, currentChar : integer;
  tempExplosion : PGameActor;
  gameOverChars : array[1..9] of gameOverChar;
  fade, waited : boolean;
  name, tempStr : string;
  breakdownRecords : array[0..6] of breakdownRecord;
begin
  fadeMusic(3000);
  if (euphoriaMusic^.playing) then euphoriaMusic^.fade(3000);
  quit := true;
  cat := nil;
  explosions := TList.create;
  resetEvents;
  setup2dMatrices;

  xDistance := -sack^.x+150;
  yDistance := -200;
  zDistance := -sack^.z+150;
  viewXRot := 30;
  rotation := -rotationToSack(-xDistance, -zDistance)+180;
  for i := 0 to 7 do explosions.add(new(PExplosion, create(sack^.x-sin(degToRad(random(360)))*60, 20+random(60), sack^.z+cos(degToRad(random(360)))*60, 2)));
  explosionSound^.play(round(25*soundEffectsVolume), 0);
  repeat
    drawStill(xDistance, yDistance, zDistance, viewXRot, rotation);
    updateExplosions(@explosions);
    refreshScreen;
  until explosions.count = 0;

  xDistance := -sack^.x-150;
  yDistance := -200;
  zDistance := -sack^.z-150;
  viewXRot := 30;
  rotation := -rotationToSack(-xDistance, -zDistance)+180;
  for i := 0 to 7 do explosions.add(new(PExplosion, create(sack^.x-sin(degToRad(random(360)))*60, 20+random(60), sack^.z+cos(degToRad(random(360)))*60, 2)));
  explosionSound^.play(round(25*soundEffectsVolume), 0);
  repeat
    drawStill(xDistance, yDistance, zDistance, viewXRot, rotation);
    updateExplosions(@explosions);
    refreshScreen;
  until explosions.count = 0;

  xDistance := -sack^.x-150;
  yDistance := -5;
  zDistance := -sack^.z+150;
  viewXRot := -20;
  rotation := -rotationToSack(-xDistance, -zDistance)+180;
  for i := 0 to 7 do explosions.add(new(PExplosion, create(sack^.x-sin(degToRad(random(360)))*60, 20+random(60), sack^.z+cos(degToRad(random(360)))*60, 2)));
  explosionSound^.play(round(25*soundEffectsVolume), 0);
  repeat
    drawStill(xDistance, yDistance, zDistance, viewXRot, rotation);
    updateExplosions(@explosions);
    refreshScreen;
  until explosions.count = 0;

  xDistance := -sack^.x;
  yDistance := -80;
  zDistance := -sack^.z+250;
  viewXRot := 0;
  rotation := -rotationToSack(-xDistance, -zDistance)+180;
  time := 0;
  timeSinceLastExplosion := 0;
  repeat
    if (timeSinceLastExplosion = 0) then
    begin
      for i := 0 to 7 do explosions.add(new(PExplosion, create(sack^.x-sin(degToRad(random(360)))*60, 20+random(60), sack^.z+cos(degToRad(random(360)))*60, 2)));
      explosionSound^.play(round((25*(1-(time/180)))*soundEffectsVolume), 0);
    end;

    drawStill(xDistance, yDistance, zDistance, viewXRot, rotation);
    updateExplosions(@explosions);

    setup2dMatrices;
    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.0, time/180);
    whiteScreen^.draw(0, 0, -1, 0, skyXScale, skyYScale);

    refreshScreen;
    time += compensation;
    timeSinceLastExplosion += compensation;
    if (timeSinceLastExplosion > 30) then timeSinceLastExplosion := 0;
  until time > 260;

  if (explosions.count > 0) then
  begin
    for i := 0 to explosions.count-1 do
    begin
      tempExplosion := PGameActor(explosions[i]);
      dispose(tempExplosion, destroy);
    end;
  end;
  explosions.clear;
  explosions.destroy;

  time := 200;
  waitTime := 0;
  waited := false;
  textHeight := tenneryBold^.height(GAME_OVER_TEXT);
  for i := 1 to 9 do
  begin
    gameOverChars[i].character := GAME_OVER_TEXT[i];
    gameOverChars[i].x := (screenWidth*0.5)-220+(40*i);
    gameOverChars[i].y := -20*i;
  end;

  repeat
    fade := true;
    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0*(time/200));
    for i := 1 to 9 do
    begin
      with gameOverChars[i] do
      begin
        y += compensation*2;
        if (y > (screenHeight*0.5)-(textHeight*0.5)) then y := (screenHeight*0.5)-(textHeight*0.5) else fade := false;
        tenneryBold^.write(character, round(x), round(y), -1);
      end;
    end;

    refreshScreen;
    if (fade) then
    begin
       if (waited) then time -= compensation else
       begin
         if (waitTime < 60) then waitTime += compensation else waited := true;
       end;
    end;
  until time < 0;

  breakdownRecords[0].text := 'Score breakdown';
  breakdownRecords[1].text := 'Drones: '+intToStr(SPAMBOT_REWARD[DRONE])+' x '+intToStr(spambotKills[DRONE])+' = '+intToStr(SPAMBOT_REWARD[DRONE]*spambotKills[DRONE]);
  breakdownRecords[2].text := 'Hackers: '+intToStr(SPAMBOT_REWARD[HACKER])+' x '+intToStr(spambotKills[HACKER])+' = '+intToStr(SPAMBOT_REWARD[HACKER]*spambotKills[HACKER]);
  breakdownRecords[3].text := 'Mechs: '+intToStr(SPAMBOT_REWARD[MECH])+' x '+intToStr(spambotKills[MECH])+' = '+intToStr(SPAMBOT_REWARD[MECH]*spambotKills[MECH]);
  breakdownRecords[4].text := 'Enemy turrets: '+intToStr(TURRET_REWARD)+' x '+intToStr(turretKills[ENEMY_TURRET])+' = '+intToStr(TURRET_REWARD*turretKills[ENEMY_TURRET]);
  breakdownRecords[5].text := 'Friendly turrets: '+intToStr(-TURRET_REWARD)+' x '+intToStr(turretKills[FRIENDLY_TURRET])+' = '+intToStr(-TURRET_REWARD*turretKills[FRIENDLY_TURRET]);
  breakdownRecords[6].text := 'Total: '+intToStr(score);

  for i := 0 to 6 do
  begin
    with breakdownRecords[i] do
    begin
      x := 30;
      if (i = 0) then y := 30 else if (i = 6) then y := 80+(30*i) else y := 50+(30*i);
      time := 60*i;
      soundPlayed := false;
    end;
  end;

  name := 'A';
  time := 0;
  waitTime := 15;
  currentChar := 1;
  waited := false; //reuse of already declared variables, rather than wasting (a tiny amount) of memory
  fade := true;
  fontShader^.use;
  fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
  repeat
    time += compensation;
    for i := 0 to 6 do
    begin
      if (time > breakdownRecords[i].time) then
      begin
        if (not breakdownRecords[i].soundPlayed) then
        begin
          bipSound^.play(round(50*soundEffectsVolume));
          breakdownRecords[i].soundPlayed := true;
        end;
        if (i = 0) then tenneryBold^.write(breakdownRecords[i].text, breakdownRecords[i].x, breakdownRecords[i].y, -1, true, 0, 0.6, 0.6)
          else tenneryBold^.write(breakdownRecords[i].text, breakdownRecords[i].x, breakdownRecords[i].y, -1, true, 0, 0.5, 0.5);
      end;
    end;

    if (time > breakdownRecords[6].time+60) then
    begin
      if (fade) then
      begin
        bipSound^.play(round(50*soundEffectsVolume));
        fade := false;
      end;

      tenneryBold^.write('Please enter your name:', 30, 320, -1, true, 0, 0.6, 0.6);
      if (waitTime >= 15) then
      begin
        if (right) then
        begin
          if (currentChar < 8) then
          begin
            currentChar += 1;
            waitTime := 0;
          end;
        end;
        if (left) then
        begin
          if (currentChar > 1) then
          begin
            currentChar -= 1;
            waitTime := 0;
          end;
        end;
        if (up) then
        begin
          name[currentChar] := chr(ord(name[currentChar])+1);
          if (ord(name[currentChar]) > ord('Z')) then name[currentChar] := 'A';
          waitTime := 0;
        end;
        if (down) then
        begin
          name[currentChar] := chr(ord(name[currentChar])-1);
          if (ord(name[currentChar]) < ord('A')) then name[currentChar] := 'Z';
          waitTime := 0;
        end;
        if (currentChar > length(name)) then name += 'A' else if (currentChar < length(name)) then setLength(name, currentChar);
      end else waitTime += compensation;

      tempStr := name;
      if (length(name) < 8) then for i := length(name)+1 to 8 do tempStr += '_';

      for i := 1 to 8 do
      begin
        if (i = currentChar) then
        begin
          fontShader^.use;
          fontShader^.setUniform4(EXTRA0_LOCATION, 0.7, 0.7, 0.7, 1.0);
          tenneryBold^.write(tempStr[i], 30+(i*45), 400, -1, true, 0, 0.8, 0.8);
          fontShader^.use;
          fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
          pictosWeb^.write('{', 30+(i*45), 360, -1, true, 0, 0.8, 0.8);
          pictosWeb^.write('}', 30+(i*45), 435, -1, true, 0, 0.8, 0.8);
        end else tenneryBold^.write(tempStr[i], 30+(i*45), 400, -1, true, 0, 0.8, 0.8);
      end;

      fontShader^.use;
      fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
      tenneryBold^.write('Press fire to continue', 30, 500, -1, true, 0, 0.7, 0.7);

      if (trigger) then waited := true;
    end;

    refreshScreen;
  until waited;

  addHighScore(name, score);
  writeHighScores;
  clearFontCache;
end;

procedure game;
var
  spambotEnum : spambotTypeEnum;
begin
  {$ifdef ENABLE_MOUSE}
  setMousePosition(round(screenWidth/2), mouseY);
  {$endif}

  music('game')^.play;

  paused := false;
  quit := false;
  respawning := false;
  money := 0;
  score := 0;
  euphoriaBonus := 1;
  soundEffectsVolume := 1;

  for spambotEnum := DRONE to MECH do spambotKills[spambotEnum] := 0;
  turretKills[ENEMY_TURRET] := 0;
  turretKills[FRIENDLY_TURRET] := 0;

  respawn;

  setup2dMatrices;

  repeat
    manageEuphoria;

    handleGameInput;

    sky^.draw(0, 0, -1000, 0, skyXScale, skyYScale);

    setup3dMatrices;
    drawFloor;
    updateGameActors;

    setup2dMatrices;

    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.0, 1.0);
    if (respawning) then tenneryBold^.write('RESPAWNING IN '+intToStr(timeLeftBeforeRespawn), 10, screenHeight-50, -1, false, 0, 0.8, 0.8);

    hud^.update;

    refreshScreen;

    if (sack^.getHealth <= 0) then loseSequence;

    if (pause) then pauseMenu;
    if (closeClicked) then quitGame;
  until quit;

  stopMusic;
  quit := false;
end;

procedure setup3dMatrices(useDefault : boolean = false);
begin
  setMatrix(MODELVIEW_MATRIX);
  copyMatrix(PERSPECTIVE_MATRIX, PROJECTION_MATRIX);
  copyMatrix(IDENTITY_MATRIX, MODELVIEW_MATRIX);

  if (respawning) then
  begin
    translateMatrix(RESPAWN_VIEW_X_DIST, RESPAWN_VIEW_Y_DIST, RESPAWN_VIEW_Z_DIST);
    rotateMatrix(RESPAWN_VIEW_ANGLE, 1, 0, 0);
  end
else
  begin
    if (useDefault) then
    begin
      translateMatrix(DEFAULT_VIEW_X_DIST, DEFAULT_VIEW_Y_DIST, DEFAULT_VIEW_Z_DIST);
      rotateMatrix(DEFAULT_VIEW_ANGLE, 1, 0, 0);
    end
  else
    begin
      translateMatrix(VIEW_X_DIST, VIEW_Y_DIST, VIEW_Z_DIST);
      rotateMatrix(VIEW_ANGLE, 1, 0, 0);
    end;
  end;
  rotateMatrix(rotation, 0, 1, 0);
  translateMatrix(xDistance, yDistance, zDistance);
end;

procedure setup2dMatrices;
begin
  copyMatrix(IDENTITY_MATRIX, MODELVIEW_MATRIX);
  spriteShader^.bind;
  copyMatrix(ORTHOGRAPHIC_MATRIX, PROJECTION_MATRIX);
end;

procedure drawFloor;
var
  i, j : integer;
begin
  flatShader^.bind;
  for i := 0 to 4 do
  begin
    for j := 0 to 4 do
    begin
      floorObject^.setPosition(i*401, 0, j*401);
      floorObject^.draw(true, true);
    end;
  end;
end;

procedure drawShadow(pClass : Pointer; shader : PShader; data : Pointer);
begin
  if (shader <> nil) then
  begin
    glActiveTexture(TEXTURE0);
    glBindTexture(GL_TEXTURE_RECTANGLE, PSprite(pClass)^.texture(0));
    pushMatrix;
      translateMatrix(tSpriteDrawParams(data^).x, 0.1, tSpriteDrawParams(data^).depth);
      rotateMatrix(tSpriteDrawParams(data^).rotation, 0.0, 1.0, 0.0);
      translateMatrix((-PSprite(pClass)^.width*tSpriteDrawParams(data^).xScale)/2, 0, (-PSprite(pClass)^.width*tSpriteDrawParams(data^).yScale)/2);
      rotateMatrix(90, 1.0, 0.0, 0.0);
      scaleMatrix(tSpriteDrawParams(data^).xScale, tSpriteDrawParams(data^).yScale, 0.0);

      glUseProgram(shader^.getProgram);
      shader^.setUniform16(MODELVIEW_LOCATION, getMatrix(MODELVIEW_MATRIX));
      shader^.setUniform16(PROJECTION_LOCATION, getMatrix(PROJECTION_MATRIX));
      shader^.setUniform1(TEXSAMPLER_LOCATION, 0);

      if (vertexArrayObjectSupported) then
      begin
        glBindVertexArray(PSprite(pClass)^.vao);
        glBindBuffer(GL_ARRAY_BUFFER, PSprite(pClass)^.vbo);
      end
    else
      begin
        glBindBuffer(GL_ARRAY_BUFFER, PSprite(pClass)^.vbo);
        glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, 0, nil);
        glEnableVertexAttribArray(0);
      end;

      glDrawArrays(GL_TRIANGLES, 0, 6);
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      if (vertexArrayObjectSupported) then glBindVertexArray(0);
    popMatrix;
  end;
end;

function up : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('w')) or joystickDpadPressed(DPAD_UP));
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('w'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickDpadPressed(DPAD_UP));
  {$endif}
end;

function down : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('s')) or joystickDpadPressed(DPAD_DOWN));
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('s'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickDpadPressed(DPAD_DOWN));
  {$endif}
end;

function left : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('a')) or joystickDpadPressed(DPAD_LEFT));
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('a'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickDpadPressed(DPAD_LEFT));
  {$endif}
end;

function right : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('d')) or joystickDpadPressed(DPAD_RIGHT));
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('d'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickDpadPressed(DPAD_RIGHT));
  {$endif}
end;

function clockwise : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('l')) or joystickButtonPressed(1));
  if (not result) then
  begin
    if (mouseX > screenWidth) then
    begin
      result := true;
      mouseMoveAmount := mouseX-round(screenWidth/2);
      setMousePosition(round(screenWidth/2), mouseY);
    end;
  end;
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('l'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickButtonPressed(1));
  {$endif}
end;

function anticlockwise : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('j')) or joystickButtonPressed(2));
  if (not result) then
  begin
    if (mouseX < screenWidth) then
    begin
      result := true;
      mouseMoveAmount := round(screenWidth/2)-mouseX;
      setMousePosition(round(screenWidth/2), mouseY);
    end;
  end;
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('j'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickButtonPressed(2));
  {$endif}
end;

function trigger : boolean;
begin
  {$ifdef NO_BUTTON_LIMIT}
  result := (keyPressed(ord('i')) or joystickButtonPressed(5) or mouseLeft);
  {$else}
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(ord('i'))) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickButtonPressed(5));
  {$endif}
end;

function pause : boolean;
begin
  result := ((configDetails[CONFIG_USING_CONTROLLER] = 0) and keyPressed(27)) or ((configDetails[CONFIG_USING_CONTROLLER] = 1) and joystickButtonPressed(7));
end;

procedure startOutline;
begin
  skeletonLineShader^.bind;
  skeletonLineShader^.use;
  skeletonLineShader^.setUniform4(TEXSAMPLER_LOCATION, 0.0, 0.0, 0.0, 1.0);
  glEnable(GL_CULL_FACE);
  glCullFace(GL_FRONT);
  glPolygonMode(GL_BACK, GL_LINE);
  glLineWidth(2);
end;

procedure stopOutline;
begin
  glLineWidth(1);
  glPolygonMode(GL_BACK, GL_FILL);
  glDisable(GL_CULL_FACE);
end;

procedure initLines;
var
  vertexArray : array[0..7] of GLfloat = (0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0);
begin
  if (vertexArrayObjectSupported) then
  begin
    glGenVertexArrays(1, @lineVao);
    glBindVertexArray(lineVao);
  end;

  glGenBuffers(1, @lineVbo);
  glBindBuffer(GL_ARRAY_BUFFER, lineVbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*length(vertexArray), @vertexArray, GL_STATIC_DRAW);

  if (vertexArrayObjectSupported) then
  begin
    glVertexAttribPointer(0, 4, GL_FLOAT, false, 0, nil);
    glEnableVertexAttribArray(0);
  end;

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  if (vertexArrayObjectSupported) then glBindVertexArray(0);
end;

procedure freeLines;
begin
  glDeleteBuffers(1, @lineVbo);
  if (vertexArrayObjectSupported) then glDeleteVertexArrays(1, @lineVao);
end;

procedure drawLine(x, y, z, length, thickness, rotation, r, g, b, a : real);
begin
  pushMatrix;
    if (vertexArrayObjectSupported) then
    begin
      glBindVertexArray(lineVao);
      glBindBuffer(GL_ARRAY_BUFFER, lineVbo);
    end
  else
    begin
      glBindBuffer(GL_ARRAY_BUFFER, lineVbo);
      glVertexAttribPointer(0, 4, GL_FLOAT, false, 0, nil);
      glEnableVertexAttribArray(0);
    end;

    setMatrix(MODELVIEW_MATRIX);
    translateMatrix(x, y, z);
    rotateMatrix(rotation, 0, 1, 0);
    scaleMatrix(length, 1, 1);

    lineShader^.use;
    lineShader^.setUniform16(MODELVIEW_LOCATION, getMatrix(MODELVIEW_MATRIX));
    lineShader^.setUniform16(PROJECTION_LOCATION, getMatrix(PROJECTION_MATRIX));
    lineShader^.setUniform4(TEXSAMPLER_LOCATION, r, g, b, a);
    glLineWidth(thickness);
    glDrawArrays(GL_LINES, 0, 2);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    if (vertexArrayObjectSupported) then glBindVertexArray(0) else glDisableVertexAttribArray(0);
    glUseProgram(0);
    glLineWidth(1);
  popMatrix;
end;

procedure bufferText(text : string; x, y, z, originX, scale, r, g, b, a : real; fontToUse : PFont);
begin
  if (textBufferCount < 512) then
  begin
    textBuffer[textBufferCount].text := text;
    textBuffer[textBufferCount].x := x;
    textBuffer[textBufferCount].y := y;
    textBuffer[textBufferCount].z := z;
    textBuffer[textBufferCount].originX := originX;
    textBuffer[textBufferCount].scale := scale;
    textBuffer[textBufferCount].r := r;
    textBuffer[textBufferCount].g := g;
    textBuffer[textBufferCount].b := b;
    textBuffer[textBufferCount].a := a;
    textBuffer[textBufferCount].fontToUse := fontToUse;
    textBufferCount += 1;
  end;
end;

procedure drawText;
var
  i : integer;
begin
  if (textBufferCount > 0) then
  begin
    for i := 0 to textBufferCount-1 do
    begin
      with textBuffer[i] do
      begin
        pushMatrix;
          translateMatrix(x+originX, y, z);

          translateMatrix(-originX, 0, 0);
          rotateMatrix(-rotation+180, 0, 1, 0);
          translateMatrix(originX, 0, 0);

          scaleMatrix(scale, scale, -scale);
          rotateMatrix(180, 0, 0, 1);
          rotateMatrix(VIEW_ANGLE, 1, 0, 0);

          fontShader^.use;
          fontShader^.setUniform4(EXTRA0_LOCATION, r, g, b, a);
          fontToUse^.write(text, 0, 0, 0);
        popMatrix;
      end;
    end;
  end;
  textBufferCount := 0;
end;

procedure bufferSprite(sprite : PSprite; x, y, z, scale, r, g, b, a : real);
begin
  if (spriteBufferCount < 512) then
  begin
    spriteBuffer[spriteBufferCount].sprite := sprite;
    spriteBuffer[spriteBufferCount].x := x;
    spriteBuffer[spriteBufferCount].y := y;
    spriteBuffer[spriteBufferCount].z := z;
    spriteBuffer[spriteBufferCount].scale := scale;
    spriteBuffer[spriteBufferCount].r := r;
    spriteBuffer[spriteBufferCount].g := g;
    spriteBuffer[spriteBufferCount].b := b;
    spriteBuffer[spriteBufferCount].a := a;
    spriteBufferCount += 1;
  end;
end;

procedure drawSprites;
var
  i : integer;
begin
  if (spriteBufferCount > 0) then
  begin
    for i := 0 to spriteBufferCount-1 do
    begin
      with spriteBuffer[i] do
      begin
        fontShader^.bind;
        fontShader^.use;
        fontShader^.setUniform4(EXTRA0_LOCATION, r, g, b, a);

        pushMatrix;
          translateMatrix(x, y, z);
          rotateMatrix(-rotation+180, 0, 1, 0);
          scaleMatrix(scale, scale, 1.0);

          sprite^.draw(0, 0, 0);
        popMatrix;
      end;
    end;
  end;
  spriteBufferCount := 0;
  tintShader^.bind;
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
end;

function hitScan(x, z, rotation, maxLength : real; var actor : PGameActor; onlyEnemies : boolean = false; ignoreCat : boolean = false) : real;
var
  xVect, zVect, xPos, zPos, xDist, zDist, dist : real;
  i : integer;
  collided : boolean = false;
  tempGameActor : PGameActor;
  tempList : ^TList;
begin
  xVect := sin(degToRad(rotation))*RAY_PRECISION;
  zVect := -cos(degToRad(rotation))*RAY_PRECISION;
  xPos := x;
  zPos := z;

  result := 0;
  actor := nil;

  if (onlyEnemies) then tempList := @spambots else tempList := @collidables;

  if (tempList^.count > 0) then
  begin
    while (result < maxLength) do
    begin
      for i := 0 to tempList^.count-1 do
      begin
        tempGameActor := PGameActor(tempList^[i]);
        if (tempGameActor^.isShort) then continue;
        xDist := xPos-tempGameActor^.x;
        if (abs(xDist) > tempGameActor^.getRadius) then continue;
        zDist := zPos-tempGameActor^.z;
        if (abs(zDist) > tempGameActor^.getRadius) then continue;
        dist := (xDist*xDist)+(zDist*zDist);
        if (dist <= tempGameActor^.getRadius*tempGameActor^.getRadius) then
        begin
          if (ignoreCat and (tempGameActor^.name[1] = PLAYER_CHARACTER)) then continue;
          collided := true;
          actor := tempGameActor;
          break;
        end;
      end;
      result += RAY_PRECISION;
      if (collided) then break;
      xPos += xVect;
      zPos += zVect;
    end;
  end;
end;

procedure setRespawnTimer;
begin
  gameActors.add(new(PRespawnTimer, create));
end;

procedure respawn;
var
  tempSpawnPoint : PSpawnPoint;
begin
  euphoriaBuildup := 0;
  yDistance := 0;
  rotation := PLAYER_RESPAWN_ROTATION;

  if (catSpawnPoints.count > 0) then
  begin
    tempSpawnPoint := PSpawnPoint(catSpawnPoints[random(catSpawnPoints.count)]);
    xDistance := -tempSpawnPoint^.x;
    zDistance := -tempSpawnPoint^.z;
    tempSpawnPoint^.spawn;
  end
else
  begin
    xDistance := PLAYER_DEFAULT_RESPAWN_X;
    zDistance := PLAYER_DEFAULT_RESPAWN_Z;
    gameActors.add(new(PCat, create(PLAYER_DEFAULT_RESPAWN_X, PLAYER_DEFAULT_RESPAWN_Z)));
  end;
  setMousePosition(round(screenWidth/2), mouseY);

  respawning := false;
  catJustDamaged := false;
end;

procedure bindDissolveMap;
begin
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, dissolveMap^.texture);
  glActiveTexture(GL_TEXTURE0);
end;

procedure bindAlternateTexture(texture : PTexture);
begin
  glActiveTexture(GL_TEXTURE1);
  if (glSlVersion < 1.5) then glBindTexture(GL_TEXTURE_2D, texture^.texture) else glBindTexture(GL_TEXTURE_2D_ARRAY, texture^.texture);
  glActiveTexture(GL_TEXTURE0);
end;

procedure drawStill;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  sky^.draw(0, 0, -1000, 0, skyXScale, skyYScale);
  setup3dMatrices;
  drawFloor;
  if (gameActors.count > 0) then
  begin
    tintShader^.bind;
    tintShader^.use;
    tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
    for i := 0 to gameActors.count-1 do
    begin
      tempGameActor := PGameActor(gameActors[i]);
      if (tempGameActor^.visible) then
      begin
        tempGameActor^.pausedDraw;
        tempGameActor^.drawShadow;
      end;
    end;

    for i := 0 to spawnPoints.count-1 do
    begin
      tempGameActor := PGameActor(spawnPoints[i]);
      if (tempGameActor^.visible) then tempGameActor^.pausedDraw;
    end;

    startOutline;
    for i := 0 to gameActors.count-1 do
    begin
      tempGameActor := PGameActor(gameActors[i]);
      if (cat = nil) then
      begin
        if (tempGameActor^.name[1] = PLAYER_CHARACTER) then continue;
      end;
      if (tempGameActor^.visible) then tempGameActor^.draw(false, true);
    end;
    for i := 0 to spawnPoints.count-1 do
    begin
      tempGameActor := PGameActor(spawnPoints[i]);
      if (tempGameActor^.visible) then tempGameActor^.draw(true, true);
    end;
    stopOutline;
  end;
end;

procedure randomDrop(x, z : real);
var
  newGameActor : PGameActor;
begin
  if (random(20) > 0) then exit;
  case random(6) of
  0 : newGameActor := new(PHealthPack, create(x, z));
  1 : newGameActor := new(PEuphoriaPack, create(x, z));
  2 : newGameActor := new(PKittyGun, create(x, z));
  3 : newGameActor := new(PAkat, create(x, z));
  4 : newGameActor := new(PBirdCatcher, create(x, z));
  5 : newGameActor := new(PAntiMech, create(x, z));
  end;
  gameActors.add(newGameActor);
end;

initialization

gameActors := TList.create;
collidables := TList.create;
spambots := TList.create;
turrets := TList.create;
spawnPoints := TList.create;
catSpawnPoints := TList.create;
textBufferCount := 0;
spriteBufferCount := 0;
usingController := false;

finalization

gameActors.destroy;
collidables.destroy;
spambots.destroy;
turrets.destroy;
spawnPoints.destroy;
catSpawnPoints.destroy;

end.

