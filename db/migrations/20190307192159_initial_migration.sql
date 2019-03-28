-- migrate:up

create table users (
  id SERIAL PRIMARY KEY,
  username TEXT UNIQUE NOT NULL,
  email TEXT UNIQUE NOT NULL,
  password TEXT,
  oauth_source TEXT,
  api_token TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE
);

create table packages (
  id SERIAL NOT NULL,
  uuid TEXT NOT NULL PRIMARY KEY,
  owner__id INT references users(id) on delete cascade,
  name TEXT NOT NULL,
  short_description TEXT NOT NULL,
  long_description TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  unique(name)
);

create table package_releases (
  id SERIAL NOT NULL,
  uuid TEXT NOT NULL PRIMARY KEY,
  package__uuid TEXT references packages(uuid) on delete cascade,
  uploader__id INT references users(id) on delete cascade,
  version TEXT NOT NULL,
  platform TEXT NOT NULL,
  executable_url TEXT NOT NULL,
  executable_signature TEXT,
  simple_executable_install TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

create table package_events (
  id SERIAL NOT NULL,
  user__id INT references users(id) on delete cascade,
  package_release__uuid TEXT references package_releases(uuid) on delete cascade,
  type TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

create table package_calls (
  id SERIAL PRIMARY KEY,
  user__id INT references users(id) on delete cascade,
  package__uuid TEXT references packages(uuid) on delete cascade,
  exit INT,
  time_ms INT,
  arg_string TEXT,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
)

-- migrate:down

drop table package_calls;
drop table package_events;
drop table package_releases;
drop table packages;
drop table users;
