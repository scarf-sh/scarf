SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: package_calls; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE package_calls (
    id integer NOT NULL,
    user__id integer,
    package__uuid text,
    exit integer,
    time_ms integer,
    arg_string text,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: package_calls_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE package_calls_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_calls_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE package_calls_id_seq OWNED BY package_calls.id;


--
-- Name: package_events; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE package_events (
    id integer NOT NULL,
    user__id integer,
    package_release__uuid text,
    type text NOT NULL,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: package_events_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE package_events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE package_events_id_seq OWNED BY package_events.id;


--
-- Name: package_releases; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE package_releases (
    id integer NOT NULL,
    uuid text NOT NULL,
    package__id text,
    uploader__id integer,
    version text NOT NULL,
    platform text NOT NULL,
    executable_url text NOT NULL,
    executable_signature text,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: package_releases_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE package_releases_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_releases_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE package_releases_id_seq OWNED BY package_releases.id;


--
-- Name: packages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE packages (
    id integer NOT NULL,
    uuid text NOT NULL,
    owner__id integer,
    name text NOT NULL,
    short_description text NOT NULL,
    long_description text,
    created_at timestamp with time zone DEFAULT now()
);


--
-- Name: packages_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE packages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: packages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE packages_id_seq OWNED BY packages.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE users (
    id integer NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    password text,
    oauth_source text,
    api_token text,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: package_calls id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_calls ALTER COLUMN id SET DEFAULT nextval('package_calls_id_seq'::regclass);


--
-- Name: package_events id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_events ALTER COLUMN id SET DEFAULT nextval('package_events_id_seq'::regclass);


--
-- Name: package_releases id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_releases ALTER COLUMN id SET DEFAULT nextval('package_releases_id_seq'::regclass);


--
-- Name: packages id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY packages ALTER COLUMN id SET DEFAULT nextval('packages_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Name: package_calls package_calls_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_calls
    ADD CONSTRAINT package_calls_pkey PRIMARY KEY (id);


--
-- Name: package_releases package_releases_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_releases
    ADD CONSTRAINT package_releases_pkey PRIMARY KEY (uuid);


--
-- Name: packages packages_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY packages
    ADD CONSTRAINT packages_name_key UNIQUE (name);


--
-- Name: packages packages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY packages
    ADD CONSTRAINT packages_pkey PRIMARY KEY (uuid);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: package_calls package_calls_package__uuid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_calls
    ADD CONSTRAINT package_calls_package__uuid_fkey FOREIGN KEY (package__uuid) REFERENCES packages(uuid) ON DELETE CASCADE;


--
-- Name: package_calls package_calls_user__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_calls
    ADD CONSTRAINT package_calls_user__id_fkey FOREIGN KEY (user__id) REFERENCES users(id) ON DELETE CASCADE;


--
-- Name: package_events package_events_package_release__uuid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_events
    ADD CONSTRAINT package_events_package_release__uuid_fkey FOREIGN KEY (package_release__uuid) REFERENCES package_releases(uuid) ON DELETE CASCADE;


--
-- Name: package_events package_events_user__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_events
    ADD CONSTRAINT package_events_user__id_fkey FOREIGN KEY (user__id) REFERENCES users(id) ON DELETE CASCADE;


--
-- Name: package_releases package_releases_package__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_releases
    ADD CONSTRAINT package_releases_package__id_fkey FOREIGN KEY (package__id) REFERENCES packages(uuid) ON DELETE CASCADE;


--
-- Name: package_releases package_releases_uploader__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY package_releases
    ADD CONSTRAINT package_releases_uploader__id_fkey FOREIGN KEY (uploader__id) REFERENCES users(id) ON DELETE CASCADE;


--
-- Name: packages packages_owner__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY packages
    ADD CONSTRAINT packages_owner__id_fkey FOREIGN KEY (owner__id) REFERENCES users(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20190307192159');
