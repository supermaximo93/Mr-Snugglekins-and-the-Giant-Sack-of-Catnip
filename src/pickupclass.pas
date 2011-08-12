unit PickupClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  PPickup = ^TPickup;
  TPickup = object(TGameActor)
  public
    destructor destroy; virtual;
    procedure init;
    procedure update; virtual;
    procedure pickup; virtual; abstract;
  end;

  PHealthPack = ^THealthPack;
  THealthPack = object(TPickup)
  public
    constructor create(destX, destZ : real);
    procedure pickup; virtual;
  end;

  PEuphoriaPack = ^TEuphoriaPack;
  TEuphoriaPack = object(TPickup)
  public
    constructor create(destX, destZ : real);
    procedure pickup; virtual;
  end;

procedure initPickups;
procedure freePickups;

implementation

uses Math, GraphicalAssetClasses, GlobalGameVariables;

var
  healthPackModel, euphoriaPackModel : PModel;

procedure initPickups;
begin
  healthPackModel := new(PModel, create('healthPack', 'assets/models/healthpack/',
                  'healthpack.smo'));
  euphoriaPackModel := new(PModel, create('euphoriaPack', 'assets/models/euphoriapack/',
                    'euphoriapack.smo'));
end;

procedure freePickups;
begin
  dispose(euphoriaPackModel, destroy);
  dispose(healthPackModel, destroy);
end;

destructor TPickup.destroy;
begin
  TGameObject.destroy;
end;

procedure TPickup.init;
begin
  dieFlag := false;
  attributes := [];
  radius := 5;
end;

procedure TPickup.update;
begin
  rotate(0, 1, 0, true);
  y_ := 5+sin(degToRad(yRotation_*2));

  if (cat <> nil) then if (collision(cat)) then pickup;

  if (visible_) then draw(true, true);
end;

constructor THealthPack.create(destX, destZ : real);
begin
  TGameObject.create('healthPack', destX, 5, destZ, healthPackModel);
  init;
end;

procedure THealthPack.pickup;
begin
  if (cat^.getHealth < cat^.getInitialHealth) then
  begin
    cat^.heal(cat^.getInitialHealth);
    dieFlag := true;
    bipSound^.play(round(50*soundEffectsVolume));
  end;
end;

constructor TEuphoriaPack.create(destX, destZ : real);
begin
  TGameObject.create('euphoriaPack', destX, 5, destZ, euphoriaPackModel);
  init;
end;

procedure TEuphoriaPack.pickup;
begin
  euphoriaBuildup += 40;
  if (euphoriaBuildup > 100) then euphoriaBuildup := 100;
  dieFlag := true;
  bipSound^.play(round(50*soundEffectsVolume));
end;

end.

