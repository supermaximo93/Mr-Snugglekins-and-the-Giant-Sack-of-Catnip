unit SpawnPointClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

//A spawn point is a blue or red striped tube thing where the cat or spambots spawn from
type
  PSpawnPoint = ^TSpawnPoint;
  TSpawnPoint = object(TGameActor)
  protected
    time : real;
  public
    destructor destroy; virtual;
    procedure init;
    procedure spawn; virtual; abstract;
  end;

  //Cat spawn points are blue
  PCatSpawnPoint = ^TCatSpawnPoint;
  TCatSpawnPoint = object(TSpawnPoint)
  public
    constructor create(destX, destZ, newRotation : real);
    destructor destroy; virtual;
    procedure update; virtual;
    procedure pausedDraw; virtual;
    procedure spawn; virtual;
  end;

  //Other spawn points are red
  PSpambotSpawnPoint = ^TSpambotSpawnPoint;
  TSpambotSpawnPoint = object(TSpawnPoint)
  public
    constructor create(destX, destZ, newRotation : real);
    procedure update; virtual;
    procedure pausedDraw; virtual;
    procedure spawn; virtual;
  end;

  //A mech spawn point needs to be much bigger than the spawn points Drones and Hackers use
  PMechSpawnPoint = ^TMechSpawnPoint;
  TMechSpawnPoint = object(TSpawnPoint)
  public
    constructor create(destX, destZ, newRotation : real);
    procedure update; virtual;
    procedure pausedDraw; virtual;
    procedure spawn; virtual;
  end;

procedure initSpawnPoints;
procedure freeSpawnPoints;

implementation

uses Display, ShaderClass, GraphicalAssetClasses, CatClass, SpambotClass,
     GlobalGameVariables;

var
  spawnPointModel : PModel;

procedure initSpawnPoints;
begin
  spawnPointModel := new(PModel, create('spawnPoint', 'assets/models/spawnpoint/',
                  'spawnpoint.smo'));
end;

procedure freeSpawnPoints;
begin
  dispose(spawnPointModel, destroy);
end;

destructor TSpawnPoint.destroy;
begin
  TGameObject.destroy;
end;

procedure TSpawnPoint.init;
begin
  dieFlag := false;
  attributes := [];
  time := 0;
  radius := 8;
end;

constructor TCatSpawnPoint.create(destX, destZ, newRotation : real);
begin
  TGameObject.create('catSpawnPoint', destX, 0, destZ, spawnPointModel);
  yRotation_ := newRotation;
  init;
  catSpawnPoints.add(@self);
end;

destructor TCatSpawnPoint.destroy;
begin
  TGameObject.destroy;
  catSpawnPoints.delete(catSpawnPoints.indexOf(@self));
end;

procedure TCatSpawnPoint.update;
begin
  time += compensation;
  if (time > 120) then time := 0;

  pausedDraw; //Using the same drawing method for gameplay and pausing is fine
end;

procedure TCatSpawnPoint.pausedDraw;
var
  texAlpha : real;
begin
  //Have a fading in and out effect by having the texture alpha vary by time
  if (time < 60) then texAlpha := time/60 else texAlpha := 1-((time*0.5)/60);
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 0.4, 0.4, 1.0, texAlpha);
  draw(true, true);
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
end;

procedure TCatSpawnPoint.spawn;
begin
  gameActors.add(new(PCat, create(x_, z_)));
end;

constructor TSpambotSpawnPoint.create(destX, destZ, newRotation : real);
begin
  TGameObject.create('spambotSpawnPoint', destX, 0, destZ, spawnPointModel);
  yRotation_ := newRotation;
  init;
end;

procedure TSpambotSpawnPoint.update;
var
  randomness, num : integer;
begin
  //Increase the chance of a Spambot spawning by decreasing the range of values
  //that the random function can pick from depending on the score
  randomness := 8;
  num := 120;
  repeat
    if (score > num) then
    begin
      randomness -= 1;
      num += 120;
    end else break;
  until randomness <= 3;
  if (time = 0) then if (random(randomness) = 1) then spawn;
  time += compensation;
  if (time > 120) then time := 0;

  pausedDraw;
end;

procedure TSpambotSpawnPoint.pausedDraw;
var
  texAlpha : real;
begin
  if (time < 60) then texAlpha := time/60 else texAlpha := 1-((time*0.5)/60);
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0, 0.0, texAlpha);
  draw(true, true);
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
end;

procedure TSpambotSpawnPoint.spawn;
begin
  //Hackers are much rarer as they are extremely annoying and can turn the tides of the game against you easily
  case random(9) of
  0..7 : gameActors.add(new(PDrone, create(x_, z_)));
  8 : if (score >= HACKER_MINIMUM_SPAWN_SCORE) then gameActors.add(new(PHacker, create(x_, z_)));
  end;
end;

constructor TMechSpawnPoint.create(destX, destZ, newRotation : real);
begin
  TGameObject.create('mechSpawnPoint', destX, 0, destZ, spawnPointModel);
  yRotation_ := newRotation;
  init;
  radius := 21.6;
  scale(2.7, 2.7, 2.7);
end;

procedure TMechSpawnPoint.update;
var
  randomness, num : integer;
begin
  if (score < MECH_MINIMUM_SPAWN_SCORE) then
  begin
    time += compensation;
    if (time > 360) then time := 0;
    pausedDraw;
    exit;
  end;

  randomness := 8;
  num := MECH_MINIMUM_SPAWN_SCORE;
  repeat
    if (score > num) then
    begin
      randomness -= 1;
      num += 200;
    end else break;
  until randomness <= 4;

  if (time = 0) then if (random(randomness) = 1) then spawn;
  time += compensation;
  if (time > 360) then time := 0;

  pausedDraw;
end;

procedure TMechSpawnPoint.pausedDraw;
var
  texAlpha : real;
begin
  if (time < 180) then texAlpha := time/180 else texAlpha := 1-((time*0.5)/180);
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0, 0.0, texAlpha);
  draw(true, true);
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
end;

procedure TMechSpawnPoint.spawn;
begin
  gameActors.add(new(PMech, create(x_, z_)));
end;

end.

