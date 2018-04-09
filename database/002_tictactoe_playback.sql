create table if not exists "tic_tac_toe_playback"
  ( "id" serial primary key
  , "created" timestamp not null
  , "game_id" text not null
  , "board" text not null
  , "x_bot" text not null
  , "o_bot" text not null
  , "result" text not null
  , constraint "unique_tic_tac_toe_playback_game_id" unique ("game_id")
  , foreign key ("x_bot") references "bot" ("public")
  , foreign key ("o_bot") references "bot" ("public")
  );

create table if not exists "tic_tac_toe_frame"
  ( "id" serial primary key
  , "game_id" text not null
  , "index" int not null
  , "board" text not null
  , "loc_x" int not null
  , "loc_y" int not null
  , "player" text not null
  , foreign key ("game_id") references "tic_tac_toe_playback" ("game_id")
  );
