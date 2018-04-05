create table if not exists "user"
  ( "id" serial primary key
  , "created" timestamp not null
  , "public" text not null
  , "email" text
  , "role" text not null
  , constraint "unique_user_public" unique ("public")
  );

create table if not exists "user_x_github"
  ( "id" serial primary key
  , "created" timestamp not null
  , "user" text not null
  , "github" text not null
  , "email" text not null
  , constraint "unique_user_x_github_user" unique("user")
  , constraint "unique_user_x_github_github" unique("github")
  , foreign key ("user") references "user" ("public")
  );

create table if not exists "bot"
  ( "id" serial primary key
  , "created" timestamp not null
  , "public" text not null
  , "name" text not null
  , constraint "unique_app_public" unique ("public")
  );

create table if not exists "user_x_bot"
  ( "id" serial primary key
  , "created" timestamp not null
  , "user" text not null
  , "bot" text not null
  , foreign key ("user") references "user" ("public")
  , foreign key ("bot") references "bot" ("public")
  );

create table if not exists "api_key"
  ( "id" serial primary key
  , "created" timestamp not null
  , "bot" text not null
  , "key" text not null
  , foreign key ("bot") references "bot" ("public")
  , constraint "unique_api_key_key" unique("key")
  );
