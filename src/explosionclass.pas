unit ExplosionClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

const
  //VECTORS : array[0..7] of array[0..2] of integer = ((-1, 1, 1), (-1, 1, -1), (-1, -1, -1), (-1, -1, 1), (1, 1, 1), (1, 1, -1), (1, -1, -1), (1, -1, 1));
  //VECTOR_LENGTH_TAKE_ONE = 7;
  //VECTOR_LENGTH_TIMES_THREE_TAKE_ONE = 23;
  VECTORS : array[0..13] of array[0..2] of integer = ((-1, 1, 1), (-1, 1, -1), (-1, -1, -1), (-1, -1, 1), (1, 1, 1), (1, 1, -1), (1, -1, -1), (1, -1, 1),
                                                     (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1));
  VECTOR_LENGTH_TAKE_ONE = 13;
  VECTOR_LENGTH_TIMES_THREE_TAKE_ONE = 41;

type
  particle = record
    x, y, z, xSpeed, ySpeed, zSpeed, size : real;
  end;

  PExplosion = ^TExplosion;
  TExplosion = object(TGameActor)
  private
    time : real;
    particles : array[0..VECTOR_LENGTH_TIMES_THREE_TAKE_ONE] of particle;
  public
    constructor create(destX, destY, destZ : real; newSize : real = 1.0);
    destructor destroy; virtual;
    procedure update; virtual;
    procedure pausedDraw; virtual;
  end;

procedure initExplosions;
procedure freeExplosions;

implementation

uses Display, GraphicalAssetClasses, ShaderClass, SoundClass, GlobalGameVariables;

procedure initExplosions;
begin
  explosionSound := new(PSound, create('explosion', 'assets/sounds/bomb-02.ogg'));
end;

procedure freeExplosions;
begin
  dispose(explosionSound, destroy);
end;

constructor TExplosion.create(destX, destY, destZ : real; newSize : real = 1.0);
var
  i, j, count : integer;
  volume, xDist, zDist, dist : real;
begin
  dieFlag := false;
  attributes := [];
  TGameObject.create('explosion', destX, destY, destZ, PModel(nil));
  count := 0;
  for i := 0 to VECTOR_LENGTH_TAKE_ONE do
  begin
    for j := 1 to 3 do
    begin
      with particles[count] do
      begin
        x := x_;
        y := y_;
        z := z_;
        xSpeed := VECTORS[i][0]*j*newSize;
        ySpeed := VECTORS[i][1]*j*newSize;
        zSpeed := VECTORS[i][2]*j*newSize;
        size := (4/j)*newSize;
      end;
      count += 1;
    end;
  end;

  if (cat <> nil) then
  begin
    xDist := x_-cat^.x;
    zDist := z_-cat^.z;
    dist := (xDist*xDist)+(zDist*zDist);
    if (dist > MAX_HEARING_DISTANCE_SQAURED) then exit;
    dist := sqrt(dist);

    volume := round(20*(100/dist));
    if (volume > 30) then volume := 30;
    explosionSound^.play(round(volume*soundEffectsVolume));
  end;
end;

destructor TExplosion.destroy;
begin
  TGameObject.destroy;
end;

procedure TExplosion.update;
var
  i : integer;
begin
  time += compensation;
  if (time > 40) then dieFlag := true;

  for i := 0 to VECTOR_LENGTH_TIMES_THREE_TAKE_ONE do
  begin
    particles[i].x += compensation*particles[i].xSpeed;
    particles[i].y += compensation*particles[i].ySpeed;
    particles[i].z += compensation*particles[i].zSpeed;
    particles[i].xSpeed -= compensation*0.02*particles[i].xSpeed;
    particles[i].ySpeed -= compensation*0.02*particles[i].ySpeed;
    particles[i].zSpeed -= compensation*0.02*particles[i].zSpeed;
    particles[i].size -= compensation*0.07*particles[i].size;
    if (visible_) then bufferSprite(smoke, particles[i].x, particles[i].y, particles[i].z, particles[i].size/5, 1.0, 1.0, 1.0, 1.0);
  end;
end;

procedure TExplosion.pausedDraw;
var
  i : integer;
begin
  for i := 0 to VECTOR_LENGTH_TIMES_THREE_TAKE_ONE do bufferSprite(smoke, particles[i].x, particles[i].y, particles[i].z, particles[i].size/5, 1.0, 1.0, 1.0, 1.0);
end;

end.
