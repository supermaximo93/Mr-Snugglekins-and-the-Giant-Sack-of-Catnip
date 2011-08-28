unit GunClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  gunTypeEnum = (KITTYGUN, AKAT, BIRDCATCHER, ANTIMECH);

  PGun = ^TGun;
  TGun = object(TGameActor)
  protected
    clip, clipSize : integer;
    lastShot, rateOfFire, lastEmpty, reloadTime, nameWidth : real;
    reloaded, drawName : boolean;
    gunType : gunTypeEnum;
  public
    shootFlag : boolean;
    destructor destroy; virtual;
    procedure init;
    procedure update; virtual;
    procedure finalUpdate;
    procedure shoot; virtual; abstract;
    function getClip : integer;
    function getClipSize : integer;
    function isReloaded : boolean;
  end;

  //Generic gun
  PKittyGun = ^TKittyGun;
  TKittyGun = object(TGun)
  public
    constructor create(destX, destZ : real);
    procedure update; virtual;
    procedure shoot; virtual;
  end;

  //Hit scan gun
  PAkat = ^TAkat;
  TAkat = object(TGun)
  private
    bulletRot, bulletLength: real;
  public
    constructor create(destX, destZ : real);
    procedure update; virtual;
    procedure shoot; virtual;
  end;

  //Shotgun
  PBirdCatcher = ^TBirdCatcher;
  TBirdCatcher = object(TGun)
  public
    constructor create(destX, destZ : real);
    procedure update; virtual;
    procedure shoot; virtual;
  end;

  //Bazooka
  PAntiMech = ^TAntiMech;
  TAntiMech = object(TGun)
  public
    constructor create(destX, destZ : real);
    procedure update; virtual;
    procedure shoot; virtual;
  end;

procedure initGuns;
procedure freeGuns;

implementation

uses Math, Display, GraphicalAssetClasses, ShaderClass, SoundClass, GlobalGameVariables, BulletClass;

var
  kittyGunModel, akatModel, birdCatcherModel, antiMechModel : PModel;
  birdCatcherSound, reloadSound : PSound;

procedure initGuns;
begin
  kittyGunModel := new(PModel, create('kittyGun', 'assets/models/kittygun/', 'kittygun.smo'));
  akatModel := new(PModel, create('akat', 'assets/models/akat/', 'akat.smo'));
  birdCatcherModel := new(PModel, create('birdCatcher', 'assets/models/birdcatcher/', 'birdcatcher.smo'));
  antiMechModel := new(PModel, create('birdCatcher', 'assets/models/antimech/', 'antimech.smo'));

  kittyGunSound := new(PSound, create('kittyGun', 'assets/sounds/pistol-01.ogg'));
  birdCatcherSound := new(Psound, create('birdCatcher', 'assets/sounds/m16.ogg'));
  reloadSound := new(PSound, create('reload', 'assets/sounds/cockgun-02.ogg'));
end;

procedure freeGuns;
begin
  dispose(reloadSound, destroy);
  dispose(birdCatcherSound, destroy);
  dispose(kittyGunSound, destroy);

  dispose(antiMechModel, destroy);
  dispose(birdCatcherModel, destroy);
  dispose(kittyGunModel, destroy);
  dispose(akatModel, destroy);
end;

destructor TGun.destroy;
begin
  TGameObject.destroy;
end;

procedure TGun.init;
begin
  dieFlag := false;
  shootFlag := false;
  reloaded := true;
  lastShot := 0.0;
  lastEmpty := 0.0;
  nameWidth := tenneryBold^.width(name_)*0.1;
  drawName := false;
  attributes := [castsShadow, rectangularShadow];
end;

procedure TGun.update;
var
  tempRadius : real;
begin
  if (cat <> nil) then
  begin
    if (cat^.gun = @self) then
    begin
      //rotate the gun to the player's orientation
      yRotation_ := -rotation+180;
      x_ := -xDistance+(sin(degToRad(yRotation_))*height_);
      y_ := 11;
      z_ := -zDistance+(cos(degToRad(yRotation_))*height_);

      //Try to shoot
      if (shootFlag) then
      begin
        shootFlag := false;
        //Fire if there's still bullets available
        if ((lastShot >= rateOfFire) and reloaded) then
        begin
          lastShot := 0.0;
          shoot;
          clip -= 1;
          //If we ran out of bullets, reload
          if (clip <= 0) then
          begin
            lastEmpty := 0.0;
            reloaded := false;
            reloadSound^.play(round(30*soundEffectsVolume), -1);
          end;
        end;
      end;
      //Shoot and reload faster if the player has Euphoria
      if (lastShot < rateOfFire) then lastShot += compensation*euphoriaBonus;
      if (lastEmpty < reloadTime) then lastEmpty += compensation*euphoriaBonus
    else if (not reloaded) then
      begin
        clip := clipSize;
        reloaded := true;
      end;
    end
  else
    begin
      //If this gun collides with the cat then this gun becomes the cat's gun
      if (collision(cat)) then
      begin
        if (cat^.gun^.gunType <> gunType) then
        begin
          cat^.gun^.dieFlag := true;
          cat^.setGun(@self);
          setPosition(-xDistance+sin(degToRad(yRotation_))*6, 11,
            -zDistance+(cos(degToRad(yRotation_))*6));

          yRotation_ := -rotation+180;
          bipSound^.play(round(50*soundEffectsVolume));
          exit;
        end;
      end;

      //If the player is in range draw the gun name
      tempRadius := radius;
      radius := 150;
      if (collision(cat)) then drawName := true;
      radius := tempRadius;

      rotate(0, 1, 0, true);
      y_ := 5+sin(degToRad(yRotation_*2));
    end;
  end
else
  begin
    rotate(0, 1, 0, true);
    y_ := 5+sin(degToRad(yRotation_*2));
  end;

  if (visible_) then shadowDot^.draw(x_, 0, z_, yRotation_, width_, height_);
end;

procedure TGun.finalUpdate;
begin
  //Draw the gun name if the cat is in range
  if (drawName) then
  begin
    bufferText(name_, x_, 20, z_, nameWidth/2, 0.1, 1.0, 1.0, 1.0,  1.0, tenneryBold);
    drawName := false;
  end;
end;

function TGun.getClip : integer;
begin
  result := clip;
end;

function TGun.getClipSize : integer;
begin
  result := clipSize;
end;

function TGun.isReloaded : boolean;
begin
  result := reloaded;
end;

constructor TKittyGun.create(destX, destZ : real);
begin
  TGameObject.create('Kitty Gun', destX, 5, destZ, kittyGunModel);
  init;
  gunType := KITTYGUN;
  rateOfFire := 5;
  reloadTime := 50;
  clipSize := 16;
  clip := 16;
  radius := 7;
  width_ := 4;
  height_ := 12;
end;

procedure TKittyGun.update;
begin
  TGun.update;
  if (visible_) then
  begin
    draw(true, true);
    finalUpdate;
  end;
end;

procedure TKittyGun.shoot;
var
  tempBullet : PBullet;
begin
  //Create a new bullet and add it to the gameActors TList
  tempBullet := new(PBullet1, create(yRotation_, x_, z_));
  tempBullet^.setDamage(tempBullet^.getDamage*euphoriaBonus);
  gameActors.add(tempBullet);
  kittyGunSound^.play(round(20*soundEffectsVolume), 0);
end;

constructor TAkat.create(destX, destZ : real);
begin
  TGameObject.create('A-KAT', destX, 5, destZ, akatModel);
  init;
  gunType := AKAT;
  rateOfFire := 2;
  reloadTime := 60;
  clipSize := 45;
  clip := 45;
  radius := 7;
  width_ := 4;
  height_ := 11;
  bulletRot := 0;
  bulletLength := 0;
end;

procedure TAkat.update;
begin
  TGun.update;
  if (visible_) then
  begin
    //Draw a line if shooting
    if (lastShot < rateOfFire) then
      drawLine(x_, 13, z_, bulletLength, 2, -bulletRot+90, 0.2, 0.2, 0.2, 1.0);
    draw(true, true);
    finalUpdate;
  end;
end;

procedure TAkat.shoot;
var
  tempGameActor : PGameActor = nil;
begin
  //This is a hitscan weapon, so do a hit scan and damage whatever the hit scan collides with
  bulletRot := rotation+((random(3)-1)*0.6);
  bulletLength := hitScan(x_, z_, bulletRot, RAY_MAX_LENGTH, tempGameActor);
  if (tempGameActor <> nil) then tempGameActor^.doDamage(3*euphoriaBonus);
  kittyGunSound^.play(round(20*soundEffectsVolume), 0);
end;

constructor TBirdCatcher.create(destX, destZ : real);
begin
  TGameObject.create('Bird Catcher', destX, 5, destZ, birdCatcherModel);
  init;
  gunType := BIRDCATCHER;
  rateOfFire := 20;
  reloadTime := 100;
  clipSize := 2;
  clip := 2;
  radius := 7;
  width_ := 4;
  height_ := 12;
end;

procedure TBirdCatcher.update;
begin
  TGun.update;
  if (visible_) then
  begin
    draw(true, true);
    finalUpdate;
  end;
end;

procedure TBirdCatcher.shoot;
var
  i : integer;
  tempBullet : PBullet;
begin
  //Create 6 bullets with a spread
  for i := 0 to 5 do
  begin
    tempBullet := new(PBullet1, create(yRotation_-2.5+i, x_, z_));
    tempBullet^.setDamage(tempBullet^.getDamage*euphoriaBonus);
    gameActors.add(tempBullet);
  end;
  birdCatcherSound^.play(round(20*soundEffectsVolume), 0);
end;

constructor TAntiMech.create(destX, destZ : real);
begin
  TGameObject.create('Anti-Mech', destX, 5, destZ, antiMechModel);
  init;
  gunType := ANTIMECH;
  rateOfFire := 1;
  reloadTime := 120;
  clipSize := 1;
  clip := 1;
  radius := 7;
  width_ := 4;
  height_ := 12;
end;

procedure TAntiMech.update;
begin
  TGun.update;
  if (visible_) then
  begin
    draw(true, true);
    finalUpdate;
  end;
end;

procedure TAntiMech.shoot;
var
  tempBullet : PBullet;
begin
  //Create a rocket and add it to gameActors
  tempBullet := new(PRocket, create(yRotation_, x_, z_));
  tempBullet^.setDamage(tempBullet^.getDamage*euphoriaBonus);
  gameActors.add(tempBullet);
  kittyGunSound^.play(round(20*soundEffectsVolume), 0);
end;

end.

