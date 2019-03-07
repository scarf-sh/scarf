-- migrate:up

create table users (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL,
  email TEXT NOT NULL,
  password TEXT,
  oauth_source TEXT,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP
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

drop table users;
drop table packages;
drop table package_events;
drop table package_calls;
