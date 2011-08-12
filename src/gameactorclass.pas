unit GameActorClass;

{$mode objfpc}{$H+}

interface

uses GraphicalAssetClasses;

type
  actorAttribute = (short, moveable, giant, hovering, castsShadow, rectangularShadow);

  PGameActor = ^TGameActor;
  TGameActor = object(TGameObject)
  protected
    health, initialHealth, radius, speed : real;
    justDamaged, visible_ : boolean;
    attributes : set of actorAttribute;
  public
    dieFlag : boolean;
    destructor destroy; virtual; abstract;
    procedure update; virtual; abstract;
    procedure pausedDraw; virtual;
    function collision(other : PGameActor) : boolean;
    procedure doDamage(amount : real);
    function getRadius : real;
    procedure setRadius(newRadius : real);
    function getSpeed : real;
    function getHealth : real;
    function getInitialHealth : real;
    procedure heal(amount : real);
    function isShort : boolean;
    function isMoveable : boolean;
    function isGiant : boolean;
    function isHovering : boolean;
    procedure updateVisibility;
    procedure setVisibility(flag : boolean);
    function visible : boolean;
    procedure drawShadow;
  end;

implementation

uses ShaderClass, GlobalGameVariables;

procedure TGameActor.pausedDraw;
begin
  draw(false, true);
end;

function TGameActor.collision(other : PGameActor) : boolean;
var
  xDist, zDist, dist, totalRadius : real;
begin
  result := false;
  totalRadius := radius+other^.radius;

  xDist := x_-other^.x_;
  if (abs(xDist) > totalRadius) then exit;
  zDist := z_-other^.z_;
  if (abs(zDist) > totalRadius) then exit;
  dist := (xDist*xDist)+(zDist*zDist);
  if (dist <= totalRadius*totalRadius) then result := true;
end;

procedure TGameActor.doDamage(amount : real);
begin
  health -= amount;
  justDamaged := true;
end;

function TGameActor.getRadius : real;
begin
  result := radius;
end;

procedure TGameActor.setRadius(newRadius : real);
begin
  radius := newRadius;
end;

function TGameActor.getSpeed : real;
begin
  result := speed;
end;

function TGameActor.getHealth : real;
begin
  result := health;
end;

function TGameActor.getInitialHealth : real;
begin
  result := initialHealth;
end;

procedure TGameActor.heal(amount : real);
begin
  health += amount;
  if (health > initialHealth) then health := initialHealth;
end;

function TGameActor.isShort : boolean;
begin
  result := (short in attributes);
end;

function TGameActor.isMoveable : boolean;
begin
  result := (moveable in attributes);
end;

function TGameActor.isGiant : boolean;
begin
  result := (giant in attributes);
end;

function TGameActor.isHovering : boolean;
begin
  result := (hovering in attributes);
end;

procedure TGameActor.updateVisibility;
begin
  {$ifdef TOP_DOWN}
  visible_ := false;
  if (x_ < -xDistance-200) then exit;
  if (x_ > -xDistance+200) then exit;
  if (z_ < -zDistance-200) then exit;
  if (z_ > -zDistance+200) then exit;
  {$endif}
  //frustum cull here
  visible_ := true;
end;

procedure TGameActor.setVisibility(flag : boolean);
begin
  visible_ := flag;
end;

function TGameActor.visible : boolean;
begin
  result := visible_;
end;

procedure TGameActor.drawShadow;
begin
  if (castsShadow in attributes) then
  begin
    if (rectangularShadow in attributes) then shadowDot^.draw(x_, 0, z_, yRotation_, width_, height_) else shadowCircle^.draw(x_, 0, z_, yRotation_, width_, width_);
  end;
end;

end.

