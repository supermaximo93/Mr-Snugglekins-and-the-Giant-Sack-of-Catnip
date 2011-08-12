unit SceneryClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  PScenery = ^TScenery;
  TScenery = object(TGameActor)
  public
    destructor destroy; virtual;
    procedure init;
    procedure update; virtual;
  end;

  PRock = ^TRock;
  TRock = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PTree = ^TTree;
  TTree = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PCrate = ^TCrate;
  TCrate = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PHouse = ^THouse;
  THouse = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PBungalow = ^TBungalow;
  TBungalow = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PBar = ^TBar;
  TBar = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PSkyScraper = ^TSkyScraper;
  TSkyScraper = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

  PStatue = ^TStatue;
  TStatue = object(TScenery)
  public
    constructor create(destX, destZ, newRotation : real);
  end;

procedure initScenery;
procedure freeScenery;

implementation

uses Classes, SysUtils, GraphicalAssetClasses, ShaderClass, GlobalGameVariables;

var
  rockModels : array[1..5] of PModel;
  treeModel, crateModel, houseModel, bungalowModel, barModel, skyScraperModel, statueModel : PModel;

procedure initScenery;
var
  i : integer;
begin
  for i := 1 to 5 do
  begin
    rockModels[i] := new(PModel, create('rock'+intToStr(i), 'assets/models/rocks/', 'rock'+intToStr(i)+'.smo'));
  end;
  treeModel := new(PModel, create('tree', 'assets/models/tree/', 'tree.smo'));
  crateModel := new(PModel, create('crate', 'assets/models/crate/', 'crate.smo'));
  houseModel := new(PModel, create('house', 'assets/models/house/', 'house.smo'));
  bungalowModel := new(PModel, create('bungalow', 'assets/models/bungalow/', 'bungalow.smo'));
  barModel := new(PModel, create('bar', 'assets/models/bar/', 'bar.smo'));
  skyScraperModel := new(PModel, create('skyScraper', 'assets/models/skyscraper/', 'skyscraper.smo'));
  statueModel := new(PModel, create('statueModel', 'assets/models/statue/', 'statue.smo'));
end;

procedure freeScenery;
var
  i : integer;
begin
  dispose(statueModel, destroy);
  dispose(skyScraperModel, destroy);
  dispose(barModel, destroy);
  dispose(bungalowModel, destroy);
  dispose(houseModel, destroy);
  dispose(crateModel, destroy);
  dispose(treeModel, destroy);
  for i := 1 to 5 do
  begin
    dispose(rockModels[i], destroy);
  end;
end;

destructor TScenery.destroy;
begin
  TGameObject.destroy;
  collidables.delete(collidables.indexOf(@self));
end;

procedure TScenery.init;
begin
  dieFlag := false;
  speed := 0;
  attributes := [];
  collidables.add(@self);
end;

procedure TScenery.update;
begin
  if (visible_) then draw(true, true);
end;

constructor TRock.create(destX, destZ, newRotation : real);
var
  num : integer;
begin
  num := random(5)+1;
  TGameObject.create(SCENERY_CHARACTER+'rock', destX, 0, destZ, rockModels[num]);
  yRotation_ := newRotation;
  init;
  radius := 10;
  if (num < 4) then attributes += [short];
end;

constructor TTree.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'tree', destX, 0, destZ, treeModel);
  yRotation_ := newRotation;
  init;
  radius := 15;
end;

constructor TCrate.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'crate', destX, 0, destZ, crateModel);
  yRotation_ := newRotation;
  init;
  radius := 12;
  scale(2, 2, 2);
end;

constructor THouse.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'house', destX, 0, destZ, houseModel);
  yRotation_ := newRotation;
  init;
  radius := 35;
end;

constructor TBungalow.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'bungalow', destX, 0, destZ, bungalowModel);
  yRotation_ := newRotation;
  init;
  radius := 35;
end;

constructor TBar.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'bar', destX, 0, destZ, barModel);
  yRotation_ := newRotation;
  init;
  radius := 35;
end;

constructor TSkyScraper.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'skyScraper', destX, 0, destZ, skyScraperModel);
  yRotation_ := newRotation;
  init;
  radius := 48;
end;

constructor TStatue.create(destX, destZ, newRotation : real);
begin
  TGameObject.create(SCENERY_CHARACTER+'statue', destX, 0, destZ, statueModel);
  yRotation_ := newRotation;
  init;
  radius := 12;
end;

end.

