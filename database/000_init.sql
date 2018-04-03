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

create table if not exists "client_credentials"
  ( "id" serial primary key
  , "created" timestamp not null
  , "user" text not null
  , "client_id" text not null
  , "client_secret" text not null
  , foreign key ("user") references "user" ("public")
  , constraint "unique_client_credentials_client_id" unique("client_id")
  );

create table if not exists "client_credentials_token"
  ( "id" serial primary key
  , "created" timestamp not null
  , "user" text not null
  , "access_token" text not null
  , "expires_in" timestamp not null
  , foreign key ("user") references "user" ("public")
  , constraint "unique_client_credentials_token_access_token" unique("access_token")
  );
