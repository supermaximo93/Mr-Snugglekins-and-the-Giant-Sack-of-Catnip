unit SightClass;

{$mode objfpc}{$H+}

interface

uses dglOpenGL, GameActorClass;

type
  //The Sight is a line that is drawn in front of the player to help them aim
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
  //Draw a line with the length from the hit scan. If the hit scan doesn't collide with anything,
  //then make the line white, otherwise red
  if (tempGameActor <> nil) then drawLine(x_, y_, z_, xScale_, 1, yRotation_, 1.0, 0.0, 0.0, 1.0)
     else drawLine(x_, y_, z_, xScale_, 1, yRotation_, 1.0, 1.0, 1.0, 1.0);
end;

end.

