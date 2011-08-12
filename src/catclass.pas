unit CatClass;

{$mode objfpc}{$H+}
{$define NO_BUTTON_LIMIT}

interface

uses GameActorClass, GunClass;

type
  PCat = ^TCat;
  TCat = object(TGameActor)
  private
    gun_ : PGun;
    timeSinceDeath, timeSinceRespawn, time : real;
    dead, respawning : boolean;
  public
    pushed : boolean;
    constructor create(destX, destZ : real);
    destructor destroy; virtual;
    procedure deathSequence;
    procedure respawnSequence;
    procedure update; virtual;
    procedure setGun(newGun : PGun);
    function gun : PGun;
  end;

procedure initCat;
procedure freeCat;

implementation

uses SysUtils, Math, Display, Input, GraphicalAssetClasses, ShaderClass, TextureClass,
     GlobalGameVariables, SpambotClass, TurretClass, SightClass;

procedure initCat;
const
  PATH = 'assets/models/cat/';
  fileNames : array[0..4] of string = (PATH+'ear_map1.jpg', PATH+'face_map2.jpg',
            PATH+'body_map1.jpg', PATH+'arm_map1.jpg', PATH+'tail_map1.jpg');
var
  i : integer;
begin
  catModel := new(PModel, create('cat', 'assets/models/cat/', 'cat.smo'));
  catWithSpannerModel := new(PModel, create('catWithSpanner', 'assets/models/catspanner/',
                      'catspanner.smo'));

  for i := 0 to 6 do lightningSprite[i] := new(PSprite, create('lightning'+intToStr(i),
      'assets/sprites/lightning/'+intToStr(i)+'.png', 0, 0, 300, 30, 1, 1, 0, 15));
  euphoriaTexture := new(PTexture, create('euphoriaTexture', TEXTURE_3D, fileNames));
end;

procedure freeCat;
var
  i : integer;
begin
  dispose(euphoriaTexture, destroy);
  for i := 0 to 6 do dispose(lightningSprite[i], destroy);
  dispose(catWithSpannerModel, destroy);
  dispose(catModel, destroy);
end;

constructor TCat.create(destX, destZ : real);
begin
  TGameObject.create(PLAYER_CHARACTER+'cat', destX, 0, destZ, catModel);
  dieFlag := false;
  radius := 7.5;
  pushed := false;
  attributes := [moveable, castsShadow];
  width_ := 0.3;
  visible_ := true;
  initialHealth := 300;
  health := initialHealth;
  timeSinceDeath := 0;
  timeSinceRespawn := 0;
  dead := false;
  respawning := true;
  rotate(0, -rotation+180, 0);
  setFrame(15);
  timeSinceRespawn := 0;
  justDamaged := false;
  collidables.add(@self);
end;

destructor TCat.destroy;
var
  i : integer;
  tempTurret : PTurret;
begin
  collidables.delete(collidables.indexOf(@self));
  cat := nil;

  if (not quit) then
  begin
    if (turrets.count > 0) then
    begin
      for i := 0 to turrets.count-1 do
      begin
        tempTurret := PTurret(turrets[i]);
        if (tempTurret^.getTarget = @self) then tempTurret^.setTarget(nil);
      end;
    end;
    setRespawnTimer;
  end;

  TGameObject.destroy;
end;

procedure TCat.deathSequence;
var
  i : integer;
  percentage : real;
  tempSpambot : PSpambot;
  tempTurret : PTurret;
begin
  if (not dead) then
  begin
    if (spambots.count > 0) then
    begin
      for i := 0 to spambots.count-1 do
      begin
        tempSpambot := PSpambot(spambots[i]);
        if (tempSpambot^.getObstacle = @self) then tempSpambot^.setObstacle(nil);
      end;
    end;
    if (turrets.count > 0) then
    begin
      for i := 0 to turrets.count-1 do
      begin
        tempTurret := PTurret(turrets[i]);
        if (tempTurret^.getTarget = @self) then tempTurret^.setTarget(nil);
      end;
    end;

    cat := nil;
    gun_^.dieFlag := true;
    sight^.dieFlag := true;
    dead := true;
  end;

  timeSinceDeath += compensation;
  if (timeSinceDeath > 80) then dieFlag := true else
  begin
    percentage := timeSinceDeath/80;
    dissolveShader^.bind;
    dissolveShader^.use;
    dissolveShader^.setUniform1(EXTRA1_LOCATION, percentage);
    bindDissolveMap;
    dissolveShader^.setUniform1(EXTRA2_LOCATION, 1);
    draw(false, true);

    tintShader^.bind;
    shadowCircle^.draw(x_, 0, z_, yRotation, 0.3*(1-percentage), 0.3*(1-percentage));
  end;
end;

procedure TCat.respawnSequence;
var
  percentage : real;
begin
  timeSinceRespawn += compensation;
  if (timeSinceRespawn > 140) then respawning := false else
  begin
    percentage := 1-(timeSinceRespawn/120);
    dissolveShader^.bind;
    dissolveShader^.use;
    dissolveShader^.setUniform1(EXTRA1_LOCATION, percentage);
    bindDissolveMap;
    dissolveShader^.setUniform1(EXTRA2_LOCATION, 1);
    draw(false, true);

    tintShader^.bind;
    if (percentage < 0) then percentage := 0;
    shadowCircle^.draw(x_, 0, z_, yRotation, 0.3*(1-percentage), 0.3*(1-percentage));
  end;
  if (not respawning) then
  begin
    cat := @self;
    gun_ := new(PKittyGun, create(0, 0));
    sight := new(PSight, create);
    gameActors.add(gun_);
    gameActors.add(sight);

    draw(false, true);
    tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
    shadowCircle^.draw(x_, 0, z_, yRotation, 0.3, 0.3);
  end;
end;

procedure TCat.update;
var
  i : integer;
  tempTurret : PTurret = nil;
  tempRadius : real;
  collided : boolean = false;
  elipsisStr : string;
begin
  time += compensation;
  if (time > 100) then time := 0;

  if (health < 1) then
  begin
    deathSequence;
    exit;
  end else if (respawning) then
  begin
    respawnSequence;
    exit;
  end;

  if (up or down or left or right {$ifndef NO_BUTTON_LIMIT}or clockwise or anticlockwise{$endif}
     or pushed) then setFrame(CAT_ANIMATION_SPEED, true) else setFrame(15);

  pushed := false;
  rotate(0, -rotation+180, 0);
  setPosition(-xDistance, 0, -zDistance);

  if (turrets.count > 0) then
  begin
    tempRadius := radius;
    radius += 10;
    for i := 0 to turrets.count-1 do
    begin
      tempTurret := PTurret(turrets[i]);
      if (collision(tempTurret)) then
      begin
        collided := true;
        break;
      end;
    end;
    radius := tempRadius;
  end;

  if (collided) then
  begin
    if (tempTurret^.isHacked) then
    begin
      if (trigger) then
      begin
        tempTurret^.hack;
        if (time < 25) then elipsisStr := '' else if (time < 50)
           then elipsisStr += '.' else if (time < 75) then elipsisStr += '..' else elipsisStr += '...';

        bufferText(TURRET_HACKING_TEXT+elipsisStr, tempTurret^.x, 30, tempTurret^.z,
          turretHackingTextWidthOverTwo, 0.1, 1.0, 1.0, 1.0, 1.0, tenneryBold);

      end else bufferText(TURRET_HACK_TEXT, tempTurret^.x, 30, tempTurret^.z,
          turretHackTextWidthOverTwo, 0.1, 1.0, 1.0, 1.0, 1.0, tenneryBold);
    end
  else
    begin
      if (trigger) then
      begin
        if (tempTurret^.readyToUpgrade) then turretUpgradeMenu(tempTurret);
      end
    else
      begin
        if ((tempTurret^.getFireLevel < TURRET_MAX_LEVEL) or
           (tempTurret^.getRangeLevel < TURRET_MAX_LEVEL) or
           (tempTurret^.getHealthLevel < TURRET_MAX_LEVEL)) then
           bufferText(TURRET_UPGRADE_TEXT, tempTurret^.x, 35, tempTurret^.z,
           turretUpgradeTextWidthOverTwo, 0.1, 1.0, 1.0, 1.0,  1.0, tenneryBold)

        else if (tempTurret^.getHealth < tempTurret^.getInitialHealth) then
           bufferText(TURRET_HEAL_TEXT, tempTurret^.x, 35, tempTurret^.z,
           turretHealTextWidthOverTwo, 0.1, 1.0, 1.0, 1.0,  1.0, tenneryBold);

        bufferText('l', tempTurret^.x+cos(degToRad(rotation))*13, 28,
          tempTurret^.z+sin(degToRad(rotation))*13, 5, 0.1, 1.0, 1.0, 0.2, 1.0, pictosWeb);

        bufferText(intToStr(tempTurret^.getFireLevel), tempTurret^.x+cos(degToRad(rotation))*19,
          28, tempTurret^.z+sin(degToRad(rotation))*19, 5, 0.1, 1.0, 1.0, 1.0,  1.0, tenneryBold);

        bufferText('''', tempTurret^.x+cos(degToRad(rotation))*3, 28,
          tempTurret^.z+sin(degToRad(rotation))*3, 5, 0.1, 0.0, 0.0, 1.0, 1.0, pictosWeb);

        bufferText(intToStr(tempTurret^.getRangeLevel), tempTurret^.x+cos(degToRad(rotation))*9,
          28, tempTurret^.z+sin(degToRad(rotation))*9, 5, 0.1, 1.0, 1.0, 1.0,  1.0, tenneryBold);

        bufferText('k', tempTurret^.x-cos(degToRad(rotation))*13, 28,
          tempTurret^.z-sin(degToRad(rotation))*13, 5, 0.1, 1.0, 0.0, 0.0, 1.0, pictosWeb);

        bufferText(intToStr(round(tempTurret^.getHealth)), tempTurret^.x-cos(degToRad(rotation))*8,
          28, tempTurret^.z-sin(degToRad(rotation))*8, 5, 0.1, 1.0, 1.0, 1.0,  1.0, tenneryBold);
      end;
    end;
  end else if (trigger) then gun^.shootFlag := true;

  tintShader^.use;
  if (euphoriaBonus > 1) then tintShader^.setUniform4(EXTRA1_LOCATION, 2.0, 2.0, 1.0, 1.0)
     else tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);

  draw(false, true);
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
  shadowCircle^.draw(x_, 0, z_, yRotation, width_, width_);

  if (justDamaged) then
  begin
    justDamaged := false;
    catJustDamaged := true;
  end else catJustDamaged := false;
end;

procedure TCat.setGun(newGun : PGun);
begin
  gun_ := newGun;
end;

function TCat.gun : PGun;
begin
  result := gun_;
end;

end.

