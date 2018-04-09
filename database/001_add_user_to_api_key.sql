alter table "api_key" add column "user" text not null;
alter table "api_key" add foreign key ("user") references "user" ("public");
