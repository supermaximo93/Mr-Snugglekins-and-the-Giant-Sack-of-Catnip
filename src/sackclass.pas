unit SackClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  //The Giant Sack of Catnip
  PSack = ^TSack;
  TSack = object(TGameActor)
    constructor create(destX, destZ, newRotation : real);
    destructor destroy; virtual;
    procedure update; virtual;
  end;

procedure initSack;
procedure freeSack;

implementation

uses GraphicalAssetClasses, GlobalGameVariables;

var
  sackModel : PModel;

procedure initSack;
begin
  sackModel := new(PModel, create('sack', 'assets/models/sack/', 'sack.smo'));
end;

procedure freeSack;
begin
  dispose(sackModel, destroy);
end;

constructor TSack.create(destX, destZ, newRotation : real);
begin
  TGameObject.create('sack', destX, 0, destZ, sackModel);
  yRotation_ := newRotation;
  scale(3, 3, 3);
  dieFlag := false;
  radius := 70;
  yRotation_ := 270;
  initialHealth := SACK_HEALTH;
  health := initialHealth;
  attributes := [castsShadow];
  collidables.add(@self);
  width_ := 3;
end;

destructor TSack.destroy;
begin
  TGameObject.destroy;
end;

procedure TSack.update;
begin
  if (visible) then
  begin
    shadowCircle^.draw(x_, 0, z_, yRotation, width_, width_);
    draw(true, true);
  end;
end;

end.

