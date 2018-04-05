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

create table if not exists "app"
  ( "id" serial primary key
  , "created" timestamp not null
  , "public" text not null
  , "name" text not null
  , constraint "unique_app_public" unique ("public")
  );

create table if not exists "user_x_app"
  ( "id" serial primary key
  , "created" timestamp not null
  , "user" text not null
  , "app" text not null
  , foreign key ("user") references "user" ("public")
  , foreign key ("app") references "app" ("public")
  );

create table if not exists "api_key"
  ( "id" serial primary key
  , "created" timestamp not null
  , "app" text not null
  , "key" text not null
  , foreign key ("app") references "app" ("public")
  , constraint "unique_api_key_key" unique("key")
  );
