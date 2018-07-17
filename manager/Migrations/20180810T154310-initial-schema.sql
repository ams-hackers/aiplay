--
-- Initial database schema
--

drop table if exists players;
drop table if exists games;
drop table if exists game_players;

create table players (
  id uuid primary key,
  display_name varchar(32) not null,
  docker_repo varchar(128) not null,
  docker_image varchar(128) not null,
  last_updated timestamptz
);

create table games (
  id serial primary key,
  started timestamptz not null,
  ended timestamptz not null,
  data jsonb
);

create table game_players (
  game_id int,
  player_id int,
  primary key (game_id, player_id)
);
