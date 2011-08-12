unit BulletClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  PBullet = ^TBullet;
  TBullet = object(TGameActor)
  protected
    xSpeed, zSpeed, damage : real;
  public
    destructor destroy; virtual;
    procedure init(direction, newSpeed : real);
    procedure update; virtual;
    procedure setDamage(amount : real);
    function getDamage : real;
  end;

  PBullet1 = ^TBullet1;
  TBullet1 = object(TBullet)
  public
    constructor create(direction, destX, destZ : real);
  end;

  PRocket = ^TRocket;
  TRocket = object(TBullet)
  private
    time : real;
  public
    constructor create(direction, destX, destZ : real);
    destructor destroy; virtual;
    procedure update; virtual;
  end;

procedure initBullets;
procedure freeBullets;

implementation

uses Math, Display, GraphicalAssetClasses, GlobalGameVariables, ExplosionClass, FloatScoreClass;

var
  bullet1Model, rocketModel : PModel;

procedure initBullets;
begin
  bullet1Model := new(PModel, create('bullet1', 'assets/models/bullet1/', 'bullet1.smo'));
  rocketModel := new(PModel, create('rocket', 'assets/models/rocket/', 'rocket.smo'));
end;

procedure freeBullets;
begin
  dispose(rocketModel, destroy);
  dispose(bullet1Model, destroy);
end;

destructor TBullet.destroy;
begin
  TGameObject.destroy;
end;

procedure TBullet.init(direction, newSpeed : real);
begin
  dieFlag := false;
  yRotation_ := direction;
  xSpeed := sin(degToRad(yRotation_))*newSpeed;
  zSpeed := cos(degToRad(yRotation_))*newSpeed;
  bindShader(flatShader);
  attributes := [moveable];
end;

procedure TBullet.update;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  setX(xSpeed, true);
  setZ(zSpeed, true);
  if ((x_ < X_LOWER_BOUND) or (x_ > X_UPPER_BOUND) or (z_ < Z_LOWER_BOUND) or (z_ > Z_UPPER_BOUND)) then
  begin
    dieFlag := true;
    exit;
  end;

  if (collidables.count > 0) then
  begin
    for i := 0 to collidables.count-1 do
    begin
      tempGameActor := PGameActor(collidables[i]);
      if (collision(tempGameActor)) then
      begin
        if (not tempGameActor^.isShort) then
        begin
          tempGameActor^.doDamage(damage);
          dieFlag := true;
          break;
        end;
      end;
    end;
  end;

  if (visible_) then draw(true, true);
end;

procedure TBullet.setDamage(amount : real);
begin
  damage := amount;
end;

function TBullet.getDamage : real;
begin
  result := damage;
end;

constructor TBullet1.create(direction, destX, destZ : real);
begin
  TGameObject.create('bullet1', destX, 12, destZ, bullet1Model);
  init(direction, 10);
  scale(0.2, 0.2, 2.0);
  damage := 5;
  radius := 2.5;
end;

constructor TRocket.create(direction, destX, destZ : real);
begin
  TGameObject.create('rocket', destX, 12, destZ, rocketModel);
  init(direction, 8);
  damage := 25;
  radius := 4;
  time := 0;
end;

destructor TRocket.destroy;
var
  i : integer;
  tempGameActor : PGameActor;
begin
  if (not quit) then
  begin
    for i := 0 to collidables.count-1 do
    begin
      tempGameActor := PGameActor(collidables[i]);
      radius := 20;
      if (collision(tempGameActor)) then
      begin
        tempGameActor^.doDamage(damage);
        radius := 10;
        if (collision(tempGameActor)) then tempGameActor^.doDamage(damage);
      end;
    end;
  end;
  gameActors.add(new(PExplosion, create(x_, y_, z_, 1)));
  TBullet.destroy;
end;

procedure TRocket.update;
begin
  TBullet.update;
  if (time = 0) then gameActors.add(new(PFloatSprite, create(smoke, x_, y_, z_, 0.1, 0, 10)));
  time += compensation;
  if (time > 2) then time := 0;
end;

end.

