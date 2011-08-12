unit SightClass;

{$mode objfpc}{$H+}

interface

uses dglOpenGL, GameActorClass;

type
  PSight = ^TSight;
  TSight = object(TGameActor)
  public
    constructor create;
    destructor destroy; virtual;
    procedure update; virtual;
  end;

implementation

uses GraphicalAssetClasses, ShaderClass, GlobalGameVariables;

constructor TSight.create;
begin
  dieFlag := false;
  radius := 0;
  TGameObject.create('sight', 0, 12, 0, PModel(nil));
  xScale_ := 750;
  attributes := [];
end;

destructor TSight.destroy;
begin
  TGameObject.destroy;
end;

procedure TSight.update;
var
  tempGameActor : PGameActor = nil;
begin
  yRotation_ := -rotation+90;
  x_ := -xDistance;
  z_ := -zDistance;
  xScale_ := hitScan(x_, z_, rotation, RAY_MAX_LENGTH, tempGameActor, false, true);
  if (tempGameActor <> nil) then drawLine(x_, y_, z_, xScale_, 1, yRotation_, 1.0, 0.0, 0.0, 1.0)
     else drawLine(x_, y_, z_, xScale_, 1, yRotation_, 1.0, 1.0, 1.0, 1.0);
end;

end.

