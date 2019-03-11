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
  id SERIAL PRIMARY KEY,
  uploader__id INT references users(id) on delete cascade,
  name TEXT NOT NULL,
  version TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT NOW()
);

create table package_events (
  id SERIAL PRIMARY KEY,
  user__id INT references users(id) on delete cascade,
  package__id INT references packages(id) on delete cascade,
  type TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT NOW()
);

create table package_calls (
  id SERIAL PRIMARY KEY,
  user__id INT references users(id) on delete cascade,
  package__id INT references packages(id) on delete cascade,
  exit INT,
  time_ms INT,
  arg_string TEXT,
  created_at TIMESTAMP DEFAULT NOW()
)

-- migrate:down

drop table package_calls;
drop table package_events;
drop table packages;
drop table users;
