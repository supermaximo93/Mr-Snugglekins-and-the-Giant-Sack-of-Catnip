unit ShopkeeperClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  shopkeeperSoundEnum = (WELCOME_SOUND, GOODBYE_SOUND, PURCHASE_SOUND);
  propEnum = (TABLE, TILL, HAT);

  //The shopkeeper who talks to you
  PShopkeeper = ^TShopkeeper;
  TShopkeeper = object(TGameActor)
  private
    timeSinceStartedTalking, timeToTalk, timeSinceLastMouthRotation : real;
    mouthOpen : boolean;
  public
    constructor create(destX, destZ : real);
    destructor destroy; virtual;
    procedure update; virtual;
    procedure talk(sound : shopkeeperSoundEnum);
  end;

  //Static props for the shop
  PProp = ^TProp;
  TProp = object(TGameActor)
  public
    constructor create(prop : propEnum; destX, destY, destZ : real);
    destructor destroy; virtual;
    procedure update; virtual;
  end;

procedure initShopkeeper;
procedure freeShopkeeper;

implementation

uses Display, GraphicalAssetClasses, TextureClass, ShaderClass, SoundClass,
     GlobalGameVariables;

const
  MOUTH_OPEN_TIME = 10;

  WELCOME_SOUND_TIME = 120;
  GOODBYE_SOUND_TIME = 60;
  PURCHASE_SOUND_TIME : array[0..2] of integer = (180, 120, 120);

var
  shopkeeperModel, tableModel, tillModel, hatModel : PModel;
  mouthOpenTexture : PTexture;
  welcomeSound, goodbyeSound : PSound;
  purchaseSound : array[0..2] of PSound;

procedure initShopkeeper;
const
  PATH = 'assets/models/shopkeeper/';
  fileNames : array[0..4] of string = (PATH+'ear_map1.jpg', PATH+'face_map2.jpg',
            PATH+'body_map1.jpg', PATH+'arm_map1.jpg', 'Filler for compatibility with GLSL versions < 1.3');
begin
  shopkeeperModel := new(PModel, create('cat', 'assets/models/shopkeeper/', 'shopkeeper.smo'));
  tableModel := new(PModel, create('table', 'assets/models/table/', 'table.smo'));
  tillModel := new(PModel, create('till', 'assets/models/till/', 'till.smo'));
  hatModel := new(PModel, create('dunceHat', 'assets/models/duncehat/', 'duncehat.smo'));
  mouthOpenTexture := new(PTexture, create('mouthOpenTexture', TEXTURE_3D, fileNames));

  welcomeSound := new(PSound, create('shopkeeperHelloWelcomeToMyShop',
               'assets/sounds/shopkeeper_hello_welcome_to_my_shop.ogg'));

  goodbyeSound := new(PSound, create('shopkeeperHaveANiceDay',
               'assets/sounds/shopkeeper_have_a_nice_day.ogg'));

  purchaseSound[0] := new(PSound, create('shopkeeperHelloWelcomeToMyShop',
                   'assets/sounds/shopkeeper_ah_great_choice_that_really_packs_a_punch.ogg'));

  purchaseSound[1] := new(PSound, create('shopkeeperHelloWelcomeToMyShop',
                   'assets/sounds/shopkeeper_nice_one_nice_one.ogg'));

  purchaseSound[2] := new(PSound, create('shopkeeperHelloWelcomeToMyShop',
                   'assets/sounds/shopkeeper_woah_be_careful_with_that.ogg'));

  cashRegisterSound := new(PSound, create('shopkeeperHelloWelcomeToMyShop',
                    'assets/sounds/cash-register-05.ogg'));
end;

procedure freeShopkeeper;
var
  i : integer;
begin
  dispose(cashRegisterSound, destroy);
  for i := 0 to 2 do dispose(purchaseSound[i], destroy);
  dispose(goodbyeSound, destroy);
  dispose(welcomeSound, destroy);

  dispose(mouthOpenTexture, destroy);
  dispose(hatModel, destroy);
  dispose(tillModel, destroy);
  dispose(tableModel, destroy);
  dispose(shopkeeperModel, destroy);
end;

constructor TShopkeeper.create(destX, destZ : real);
begin
  TGameObject.create('shopkeeper', destX, -0.5, destZ, shopkeeperModel);
  timeSinceStartedTalking := 0;
  timeToTalk := 0;
  timeSinceLastMouthRotation := 0;
  mouthOpen := false;
  alternateTextureShader^.use;
  alternateTextureShader^.setUniform1(EXTRA1_LOCATION, 1);
  attributes := [castsShadow];
end;

destructor TShopkeeper.destroy;
begin
  TGameObject.destroy;
end;

procedure TShopkeeper.update;
begin
  if ((timeToTalk = 0) or (currentAnimation = 1)) then
  begin
    if (currentAnimation = 0) then setFrame(0.5, true) else setFrame(1, true);
  end;

  if (timeSinceStartedTalking >= timeToTalk) then
  begin
    timeToTalk := 0;
    mouthOpen := false;
  end
else
  begin
    //If the shopkeeper has not exceeded the time he needs to say a phrase, make the
    //mouth open and close
    timeSinceStartedTalking += compensation;
    timeSinceLastMouthRotation += compensation;
    if ((timeSinceStartedTalking >= timeToTalk-10) and (not mouthOpen)) then
    begin
      mouthOpen := true;
      timeSinceLastMouthRotation := 0;
    end;
    if (timeSinceLastMouthRotation >= MOUTH_OPEN_TIME) then
    begin
      timeSinceLastMouthRotation := 0;
      mouthOpen := not mouthOpen;
    end;
  end;

  shopkeeperModel^.bindShader(alternateTextureShader);
  alternateTextureShader^.use;
  alternateTextureShader^.setUniform4(EXTRA3_LOCATION, 1.0, 1.0, 1.0, 1.0);
  if (mouthOpen) then
  begin
    bindAlternateTexture(mouthOpenTexture);
    alternateTextureShader^.setUniform1(EXTRA2_LOCATION, 1);
  end else alternateTextureShader^.setUniform1(EXTRA2_LOCATION, 0);

  draw(false, true);

  shopkeeperModel^.bindShader(nil);
  shadowCircle^.draw(x_, 0, z_, yRotation, 0.3, 0.3);
end;

procedure TShopkeeper.talk(sound : shopkeeperSoundEnum);
var
  num : integer;
begin
  if (sound = PURCHASE_SOUND) then cashRegisterSound^.play(round(40*soundEffectsVolume), 2);
  if ((timeToTalk <> 0) and (sound <> GOODBYE_SOUND)) then exit;
  case sound of
    WELCOME_SOUND :
      begin
        timeToTalk := WELCOME_SOUND_TIME;
        welcomeSound^.play(round(100*soundEffectsVolume), 1);
      end;
    GOODBYE_SOUND :
      begin
        timeToTalk := GOODBYE_SOUND_TIME;
        goodbyeSound^.play(round(100*soundEffectsVolume), 1);
      end;
    PURCHASE_SOUND :
      begin
        num := random(3); //Pick a random phrase when the player purchases an item
        timeToTalk := PURCHASE_SOUND_TIME[num];
        purchaseSound[num]^.play(round(100*soundEffectsVolume), 1);
      end;
  end;
  timeSinceStartedTalking := 0;
  timeSinceLastMouthRotation := 0;
  mouthOpen := true;
end;

constructor TProp.create(prop : propEnum; destX, destY, destZ : real);
var
  modelToUse : PModel;
begin
  attributes := [];
  case prop of
  TABLE :
    begin
      modelToUse := tableModel;
      width_ := 38;
      height_ := 12;
      attributes += [castsShadow, rectangularShadow];
    end;
  TILL :
    begin
      modelToUse := tillModel;
      width_ := 0;
    end;
  HAT :
    begin
      modelToUse := hatModel;
      width_ := 0;
    end;
  end;
  TGameObject.create('table', destX, destY, destZ, modelToUse);
end;

destructor TProp.destroy;
begin
  TGameObject.destroy;
end;

procedure TProp.update;
begin
  model_^.bindShader(modelShader);
  draw(true, true);
  model_^.bindShader(nil);
  if (width_ > 0) then shadowDot^.draw(x_, 0, z_, yRotation_, width_, height_)
end;

end.

