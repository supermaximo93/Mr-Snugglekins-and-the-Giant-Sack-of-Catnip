unit InitialisationAndFreeing;

{$mode objfpc}{$H+}

interface

//Loads screen configuration details, initialises SuperMaximo GameLibrary subsystems,
//makes sure a few settings are correct and then loads up the game assets
procedure initEverything;

//Frees all of the game assets and closes all of the SuperMaximo GameLibrary subsystems
procedure freeEverything;

//Loads up OpenGL Shading Language shaders, while compensating for older hardware.
//Uses the SuperMaximo GameLibrary Shader class
procedure initShaders;

//Loads up fonts used in the game and sets the colour setting of the shader used for
//rendering fonts. Uses the SuperMaximo GameLibrary Font class
procedure initFonts;

//Initialises SuperMaximo GameLibrary Audio and loads up sounds and music
procedure initSounds;

//Frees the sound and music and closes the Audio subsystem
procedure freeSounds;

//Runs the init procedures of each unit containing classes to load up their assets
//and loads a few assets that do not require object-orienting. Some global variables
//that are used elsewhere are also set
procedure loadGameAssets;

//Disposes of all assets created during the loadGameAssets procedure, and also the fonts
procedure freeGameAssets;

//Checks if the 'savefiles' directory exists, and if it does not, then the directory is
//created. Likewise, if 'config.txt' does not exist, it is created, otherwise the
//configuration details are loaded from the file.
procedure loadConfigDetails;

//Loads a map specified by mapToLoad from the 'assets/maps' directory. The file is
//read linearly, and assets are placed on the map according to the details in the file
procedure loadMap(mapToLoad : string);

//Disposes of all objects created during map loading or gameplay
procedure freeMap;

//Frees everything and halts the program. Used when quiting while in the middle of
//gameplay
procedure quitGame;

implementation

uses SysUtils, TypInfo, SMSDL, Display, Input, Audio, ShaderClass, FontClass, SoundClass,
  MusicClass, TextureClass, GraphicalAssetClasses, CatClass, ShopkeeperClass,
  SpambotClass, GunClass, BulletClass, ExplosionClass, SceneryClass,
  TurretClass, SackClass, HudClass, GameActorClass, GlobalGameVariables,
  PickupClass, LevelEditor, SpawnPointClass;

procedure initEverything;
var
  fullscreen : boolean;
begin
  loadConfigDetails;
  if (configDetails[CONFIG_FULLSCREEN] = 1) then fullscreen := true else fullscreen := false;

  initSDL(SDL_INIT_EVERYTHING);
  initDisplay(configDetails[CONFIG_SCREEN_WIDTH], configDetails[CONFIG_SCREEN_HEIGHT],
    1000, 60, fullscreen, 'Mr Snugglekins and the Giant Sack of Catnip');
  setIdealFramerate(60);
  initInput;
  initShaders;
  initFonts;
  initSounds;

  enableBlending(SRC_ALPHA, ONE_MINUS_SRC_ALPHA);
  hideCursor;
  randomize;

  loadGameAssets;
end;

procedure freeEverything;
begin
  quit := true;
  freeGameAssets;
  destroyAllShaders;
  dispose(dissolveMap, destroy);
  freeSounds;
  quitFont;
  quitInput;
  quitDisplay;
  quitSDL;
  writeln('Quit successfully');
  wait(200);
end;

procedure initShaders;
const
  spriteEnums : array[0..0] of integer = (VERTEX_ATTRIBUTE);
  spriteAttrs : array[0..0] of string = ('vertex');
  modelEnums : array[0..9] of integer = (VERTEX_ATTRIBUTE, NORMAL_ATTRIBUTE, COLOR0_ATTRIBUTE,
             COLOR1_ATTRIBUTE, COLOR2_ATTRIBUTE, TEXTURE0_ATTRIBUTE, EXTRA0_ATTRIBUTE,
             EXTRA1_ATTRIBUTE, EXTRA2_ATTRIBUTE, EXTRA3_ATTRIBUTE);
  modelAttrs : array[0..9] of string = ('vertex', 'normal', 'ambientColor', 'diffuseColor',
             ' specularColor', 'texCoords', 'mtlNum', 'hasTexture', 'shininess', 'alpha');
  fileNames : array[0..0] of string = ('assets/sprites/dissolve_map.jpg');
var
  glSlPath : string;
begin
  if (glSlVersion >= 1.5) then
  begin
    glSlPath := '150/';
    writeln('Compiling GLSL version 1.5 shaders');
  end
else
  begin
    glSlPath := '110/';
    writeln('Compiling GLSL version 1.1 shaders');
  end;

  //A shader to display 2D images that don't need any colour tinting
  spriteShader := addShader('spriteShader', 'assets/shaders/'+glSlPath+'sprite_vertex_shader.vs',
               'assets/shaders/'+glSlPath+'sprite_fragment_shader.fs', spriteEnums, spriteAttrs);
  spriteShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  spriteShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  spriteShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');

  //A shader to draw the shadows nicely on the ground
  shadowShader := addShader('shadowShader', 'assets/shaders/'+glSlPath+'sprite_vertex_shader.vs',
               'assets/shaders/'+glSlPath+'shadow_fragment_shader.fs', spriteEnums, spriteAttrs);
  shadowShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  shadowShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  shadowShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');

  //A shader to draw models that don't need tinting but do need lighting
  modelShader := addShader('modelShader', 'assets/shaders/'+glSlPath+'skeleton_vertex_shader.vs',
              'assets/shaders/'+glSlPath+'skeleton_fragment_shader.fs', modelEnums, modelAttrs);
  modelShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  modelShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  modelShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  modelShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');
  if (glSlVersion < 1.5) then modelShader^.setUniformLocation(TEXCOMPAT_LOCATION, 'materialCount');

  //A shader to draw models with no lighting effects or shading
  flatShader := addShader('flatShader', 'assets/shaders/'+glSlPath+'model_vertex_shader.vs',
             'assets/shaders/'+glSlPath+'model_fragment_shader.fs', modelEnums, modelAttrs);
  flatShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  flatShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  flatShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');

  //A shader to draw lines in 3D space
  lineShader := addShader('lineShader', 'assets/shaders/'+glSlPath+'line_vertex_shader.vs',
             'assets/shaders/'+glSlPath+'line_fragment_shader.fs', spriteEnums, spriteAttrs);
  lineShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  lineShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  lineShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'color');

  //A shader to draw lines around a 3D model for a cartoon effect
  skeletonLineShader := addShader('skeletonLineShader', 'assets/shaders/'+glSlPath+'skeleton_line_vertex_shader.vs',
                     'assets/shaders/'+glSlPath+'line_fragment_shader.fs', spriteEnums, spriteAttrs);
  skeletonLineShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  skeletonLineShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  skeletonLineShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'color');
  skeletonLineShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');

  //A shader for drawing fonts with a colour tint. This shader is also useful for drawing regular tinted sprites.
  fontShader := addShader('fontShader', 'assets/shaders/'+glSlPath+'sprite_vertex_shader.vs',
             'assets/shaders/'+glSlPath+'font_fragment_shader.fs', spriteEnums, spriteAttrs);
  fontShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  fontShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  fontShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  fontShader^.setUniformLocation(EXTRA0_LOCATION, 'color');

  //A shader for drawing a 3D model, with lighting effects and colour tinting
  tintShader := addShader('tintShader', 'assets/shaders/'+glSlPath+'skeleton_vertex_shader.vs',
             'assets/shaders/'+glSlPath+'tint_fragment_shader.fs', modelEnums, modelAttrs);
  tintShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  tintShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  tintShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  tintShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');
  tintShader^.setUniformLocation(EXTRA1_LOCATION, 'tint');
  if (glSlVersion < 1.5) then tintShader^.setUniformLocation(TEXCOMPAT_LOCATION, 'materialCount');

  //A shader for drawing a 3D model with a dissolve effect that disintegrates the model over time, using
  //the 'percentage' uniform to specify the amount of disintegration
  dissolveShader := addShader('dissolveShader', 'assets/shaders/'+glSlPath+'skeleton_vertex_shader.vs',
                 'assets/shaders/'+glSlPath+'dissolve_fragment_shader.fs', modelEnums, modelAttrs);
  dissolveShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  dissolveShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  dissolveShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  dissolveShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');
  dissolveShader^.setUniformLocation(EXTRA1_LOCATION, 'percentage');
  dissolveShader^.setUniformLocation(EXTRA2_LOCATION, 'dissolveMap');
  if (glSlVersion < 1.5) then dissolveShader^.setUniformLocation(TEXCOMPAT_LOCATION, 'materialCount');

  //A shader that chops off a portion of a sprite according the value of cutoff, acting vertically
  drainVerticalShader := addShader('drainVerticalShader', 'assets/shaders/'+glSlPath+'sprite_vertex_shader.vs',
                      'assets/shaders/'+glSlPath+'drain_vertical_fragment_shader.fs', spriteEnums, spriteAttrs);
  drainVerticalShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  drainVerticalShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  drainVerticalShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  drainVerticalShader^.setUniformLocation(EXTRA0_LOCATION, 'color');
  drainVerticalShader^.setUniformLocation(EXTRA1_LOCATION, 'cutoff');

  //A shader that chops off a portion of a sprite according the value of cutoff, acting horizontally
  drainHorizontalShader := addShader('drainHorizontalShader', 'assets/shaders/'+glSlPath+'sprite_vertex_shader.vs',
                        'assets/shaders/'+glSlPath+'drain_horizontal_fragment_shader.fs', spriteEnums, spriteAttrs);
  drainHorizontalShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  drainHorizontalShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  drainHorizontalShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  drainHorizontalShader^.setUniformLocation(EXTRA0_LOCATION, 'color');
  drainHorizontalShader^.setUniformLocation(EXTRA1_LOCATION, 'cutoff');

  //A shader for drawing a 3D model with lighting and a colour tint, while using another texture to generate
  //a worn look, or if the 'severeWear' uniform equals 1, to create holes in the model to give a truly battered look
  tintAndWearShader := addShader('tintAndWearShader', 'assets/shaders/'+glSlPath+'skeleton_vertex_shader.vs',
                    'assets/shaders/'+glSlPath+'tint_and_wear_fragment_shader.fs', modelEnums, modelAttrs);
  tintAndWearShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  tintAndWearShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  tintAndWearShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  tintAndWearShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');
  tintAndWearShader^.setUniformLocation(EXTRA1_LOCATION, 'tint');
  tintAndWearShader^.setUniformLocation(EXTRA2_LOCATION, 'wearMap');
  tintAndWearShader^.setUniformLocation(EXTRA3_LOCATION, 'severeWear');
  if (glSlVersion < 1.5) then tintAndWearShader^.setUniformLocation(TEXCOMPAT_LOCATION, 'materialCount');

  //A shader for drawing a 3D model with lighting and a colour tint that allows switching between two different
  //textures. This shader is used in the shop to make it look like the shopkeeper is talking by switching two
  //different face textures
  alternateTextureShader := addShader('alternateTextureShader', 'assets/shaders/'+glSlPath+'skeleton_vertex_shader.vs',
                         'assets/shaders/'+glSlPath+'alternate_texture_fragment_shader.fs', modelEnums, modelAttrs);
  alternateTextureShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
  alternateTextureShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
  alternateTextureShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
  alternateTextureShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');
  alternateTextureShader^.setUniformLocation(EXTRA1_LOCATION, 'alternateColorMap');
  alternateTextureShader^.setUniformLocation(EXTRA2_LOCATION, 'useAlternateTexture');
  alternateTextureShader^.setUniformLocation(EXTRA3_LOCATION, 'tint');
  if (glSlVersion < 1.5) then alternateTextureShader^.setUniformLocation(TEXCOMPAT_LOCATION, 'materialCount');

  //Load up the texture for the dissolve and tintAndWear shaders
  dissolveMap := new(PTexture, create('dissolveMap', TEXTURE_2D, fileNames));
end;

procedure initFonts;
begin
  initFont(fontShader);
  tenneryBold := new(PFont, create('tenneryBold', 'assets/fonts/tennerybold.ttf', 50));
  pictosWeb := new(PFont, create('pictosWeb', 'assets/fonts/pictos-web.ttf', 50));
  fontShader^.use;
  fontShader^.setUniform4(EXTRA0_LOCATION, 1, 1, 1, 1);
end;

procedure initSounds;
begin
  initAudio;
  addMusic('menu', 'assets/music/menu.ogg');
  addMusic('game', 'assets/music/game.ogg');
  musicVolume(50);
  euphoriaSequenceSound := new(PSound, create('euphoriaSequence', 'assets/sounds/thunder-03.ogg'));
  euphoriaMusic := new(PSound, create('euphoriaMusic', 'assets/music/euphoria.ogg'));
  bipSound := new(PSound, create('bip', 'assets/sounds/beep-03.ogg'));
end;

procedure freeSounds;
begin
  dispose(bipSound, destroy);
  dispose(euphoriaMusic, destroy);
  dispose(euphoriaSequenceSound, destroy);
  destroyMusic('menu');
  destroyMusic('game');
  quitAudio;
end;

procedure loadGameAssets;
begin
  initCat;
  initShopkeeper;
  initSpambots;
  initGuns;
  initBullets;
  initExplosions;
  initLines;
  initScenery;
  initTurrets;
  initSack;
  initPickups;
  initSpawnPoints;

  sky := new(PSprite, create('sky', 'assets/sprites/sky.png', 0, 0, 800, 600));
  whiteScreen := new(PSprite, create('redScreen', 'assets/sprites/white.png', 0, 0, 800, 600));
  whiteScreen^.bindShader(fontShader);
  skyXScale := (1/800)*screenWidth;
  skyYScale := (1/600)*screenHeight;

  smoke := new(PSprite, create('smoke', 'assets/sprites/smoke.png', 0, 0, 50, 50, 1, 1, 25, 25));

  shadowCircle := new(PSprite, create('shadow', 'assets/sprites/shadow_circle.png', 0, 0, 50, 50));
  shadowCircle^.bindShader(shadowShader);
  shadowCircle^.bindCustomDrawFunction(@drawShadow);
  shadowDot := new(PSprite, create('shadow', 'assets/sprites/shadow_dot.png', 0, 0, 1, 1));
  shadowDot^.bindShader(shadowShader);
  shadowDot^.bindCustomDrawFunction(@drawShadow);

  floorModel := new(PModel, create('floor', 'assets/models/floor/', 'floor.smo'));
  floorObject := new(PGameObject, create('floor', 0, 1.2, 0, floorModel));

  turretHackTextWidthOverTwo := (tenneryBold^.width(TURRET_HACK_TEXT)*0.1)*0.5;
  turretUpgradeTextWidthOverTwo := (tenneryBold^.width(TURRET_UPGRADE_TEXT)*0.1)*0.5;
  turretHealTextWidthOverTwo := (tenneryBold^.width(TURRET_HEAL_TEXT)*0.1)*0.5;
  turretHackingTextWidthOverTwo := (tenneryBold^.width(TURRET_HACKING_TEXT)*0.1)*0.5;

  hud := new(PHud, create);
end;

procedure freeGameAssets;
begin
  dispose(hud, destroy);
  dispose(floorObject, destroy);
  dispose(floorModel, destroy);
  dispose(smoke, destroy);
  dispose(sky, destroy);


  freeSpawnPoints;
  freePickups;
  freeBullets;
  freeGuns;
  freeSpambots;
  freeExplosions;
  freeLines;
  freeScenery;
  freeTurrets;
  freeSack;
  freeCat;
  freeShopkeeper;

  dispose(tenneryBold, destroy);
  dispose(pictosWeb, destroy);
end;

procedure loadConfigDetails;
var
  configFile : text;
  tempStr : string;
  i : integer = 0;
begin
  if (directoryExists('assets/savefiles')) then
  begin
    if (fileExists('assets/savefiles/config.txt')) then
    begin
      assign(configFile, 'assets/savefiles/config.txt');
      reset(configFile);
      repeat
        if (i > CONFIG_USING_CONTROLLER) then break;
        readln(configFile, tempStr);
        configDetails[i] := strToInt(tempStr);
        i += 1;
      until eof(configFile);
      close(configFile);
      if ((joystickCount < 1) and (configDetails[CONFIG_USING_CONTROLLER] = 1))
         then configDetails[CONFIG_USING_CONTROLLER] := 0;
    end
  else
    begin
      configDetails[CONFIG_SCREEN_WIDTH] := 800;
      configDetails[CONFIG_SCREEN_HEIGHT] := 600;
      configDetails[CONFIG_FULLSCREEN] := 0;
      configDetails[CONFIG_USING_CONTROLLER] := 0;
      assign(configFile, 'assets/savefiles/config.txt');
      rewrite(configFile);
      for i := CONFIG_SCREEN_WIDTH to CONFIG_USING_CONTROLLER do
      begin
        writeln(configFile, configDetails[i]);
      end;
      close(configFile);
    end;
  end
else
  begin
    mkdir('assets/savefiles');
    loadConfigDetails;
  end;
end;

procedure loadMap(mapToLoad : string);
var
  tempStr : string;
  mapFile : text;
  mapPieceToLoad : mapPieceEnum;
  mapPieceX, mapPieceZ, mapPieceRotation : integer;
begin
  if (directoryExists('assets/maps')) then
  begin
    if (rightStr(mapToLoad, 4) <> '.map') then mapToLoad += '.map';
    if (fileExists('assets/maps/'+mapToLoad)) then
    begin
      assign(mapFile, 'assets/maps/'+mapToLoad);
      reset(mapFile);

      readln(mapFile, tempStr);
      writeln('Loading map '''+tempStr+'''...');

      repeat
        readln(mapFile, tempStr);
        mapPieceToLoad := mapPieceEnum(GetEnumValue(TypeInfo(mapPieceEnum), tempStr));

        readln(mapFile, tempStr);
        mapPieceX := strToInt(tempStr);
        readln(mapFile, tempStr);
        mapPieceZ := strToInt(tempStr);
        readln(mapFile, tempStr);
        mapPieceRotation := strToInt(tempStr);

        case mapPieceToLoad of
        ROCK : gameActors.add(new(PRock, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        TREE : gameActors.add(new(PTree, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        CRATE : gameActors.add(new(PCrate, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        HOUSE : gameActors.add(new(PHouse, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        BUNGALOW : gameActors.add(new(PBungalow, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        BAR : gameActors.add(new(PBar, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        SKYSCRAPER : gameActors.add(new(PSkyScraper, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        STATUE : gameActors.add(new(PStatue, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        SACK_OF_CATNIP :
          begin
            sack := new(PSack, create(mapPieceX, mapPieceZ, mapPieceRotation));
            gameActors.add(sack);
          end;
        CAT_SPAWNPOINT : spawnPoints.add(new(PCatSpawnPoint, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        SPAMBOT_SPAWNPOINT : spawnPoints.add(new(PSpambotSpawnPoint, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        MECH_SPAWNPOINT : spawnPoints.add(new(PMechSpawnPoint, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        end;
      until eof(mapFile);

      close(mapFile);
    end else writeln('Map '+mapToLoad+' does not exist');
  end else writeln('No maps to load');
end;

procedure freeMap;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  if (gameActors.count > 0) then
  begin
    for i := 0 to gameActors.count-1 do
    begin
      tempGameActor := PGameActor(gameActors[i]);
      dispose(tempGameActor, destroy);
    end;
  end;

  if (spawnPoints.count > 0) then
  begin
    for i := 0 to spawnPoints.count-1 do
    begin
      tempGameActor := PGameActor(spawnPoints[i]);
      dispose(tempGameActor, destroy);
    end;
  end;

  gameActors.clear;
  collidables.clear;
  spambots.clear;
  turrets.clear;
  spawnPoints.clear;
  catSpawnPoints.clear;
end;

procedure quitGame;
begin
  freeMap;
  freeEverything;
  halt;
end;

end.

