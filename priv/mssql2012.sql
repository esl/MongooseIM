GO
CREATE TABLE [dbo].[test_types](
    [unicode] [nvarchar](max),
    [unicode250] [nvarchar](250),
    [binary_data_8k] [varbinary](8000),
    [binary_data_65k] [varbinary](max),
    [binary_data_16m] [varbinary](max), -- varbinary(max) is 2^31-1 bytes
    [ascii_char] char(1),
    [ascii_string] varchar(250), -- limited usage, base64-like stuff
    [int32] [int],
    [int64] [bigint],
    [int8] tinyint,
    [enum_char] [nvarchar](1),
    [bool_flag] smallint
)
GO

/****** Object:  Table [dbo].[last]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[last](
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[seconds] [int] NOT NULL,
	[state] [nvarchar](max) NOT NULL,
 CONSTRAINT [PK_last_username] PRIMARY KEY CLUSTERED
(
	[server] ASC,
	[username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE INDEX i_last_server_seconds ON last (server, seconds);
GO

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_config]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_config](
	[user_id] [bigint] NOT NULL,
	[remote_jid] [nvarchar](250) NOT NULL,
	[behaviour] [nvarchar](1) NOT NULL
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_message]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_message](
	[id] [bigint] NOT NULL,
	[user_id] [bigint] NOT NULL,
	[from_jid] [nvarchar](250) NOT NULL,
	[remote_bare_jid] [nvarchar](250) NOT NULL,
	[remote_resource] [nvarchar](250) NOT NULL,
	[direction] [nvarchar](1) NOT NULL,
	[message] [varbinary](max) NOT NULL,
	[search_body] [nvarchar](max) NOT NULL,
	[origin_id] [nvarchar](250) NULL,
	[is_groupchat] [smallint] NOT NULL,
 CONSTRAINT [PK_mam_message_user_id] PRIMARY KEY CLUSTERED
(
	[user_id] ASC,
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE INDEX i_mam_message_username_jid_id ON mam_message (user_id, remote_bare_jid, id);
GO

CREATE INDEX i_mam_message_username_jid_origin_id ON mam_message (user_id, remote_bare_jid, origin_id);
GO

SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_muc_message]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
/* id is not unique across the table, because we create copies of the same
 * archive for different users in our tests */
CREATE TABLE [dbo].[mam_muc_message](
	[id] [bigint] NOT NULL,
	[room_id] [bigint] NOT NULL,
	[sender_id] [bigint] NOT NULL,
	[nick_name] [nvarchar](250) NOT NULL,
	[message] [varbinary](max) NOT NULL,
	[search_body] [nvarchar](max) NOT NULL,
	[origin_id] [nvarchar](250) NULL,
 CONSTRAINT [PK_mam_muc_message_id] PRIMARY KEY CLUSTERED
(
    [room_id] ASC,
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE INDEX i_mam_muc_message_sender_id ON mam_muc_message(sender_id);
GO

CREATE INDEX i_mam_muc_message_room_id_sender_id_origin_id ON mam_muc_message (room_id, sender_id, origin_id);
GO

SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_server_user]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[mam_server_user](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[server] [nvarchar](250) NOT NULL,
	[user_name] [nvarchar](250) NOT NULL,
 CONSTRAINT [PK_mam_server_user_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [mam_server_user$uc_mam_server_user_name] UNIQUE NONCLUSTERED
(
	[server] ASC,
	[user_name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[offline_message]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[offline_message](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[timestamp] [bigint] NOT NULL,
	[expire] [bigint] NULL,
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[from_jid] [nvarchar](250) NOT NULL,
	[packet] [nvarchar](max) NOT NULL,
	[permanent_fields] [varbinary](max),
 CONSTRAINT [PK_offline_message_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

CREATE TABLE auth_token (
  owner NVARCHAR(250) NOT NULL PRIMARY KEY,
  seq_no BIGINT NOT NULL,
);

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[privacy_default_list]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[privacy_default_list](
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[name] [nvarchar](250) NOT NULL,
 CONSTRAINT [PK_privacy_default_list_username] PRIMARY KEY CLUSTERED
(
	[server], [username]
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[privacy_list]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[privacy_list](
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[name] [nvarchar](250) NOT NULL,
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_privacy_list] PRIMARY KEY CLUSTERED
(
	[server] ASC,
	[username] ASC,
	[name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[privacy_list_data]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[privacy_list_data](
	[id] [bigint] NULL,
	[t] [char](1) NOT NULL,
	[value] [nvarchar](max) NOT NULL,
	[action] [char](1) NOT NULL,
	[ord] [int] NOT NULL,
	[match_all] [smallint] NOT NULL,
	[match_iq] [smallint] NOT NULL,
	[match_message] [smallint] NOT NULL,
	[match_presence_in] [smallint] NOT NULL,
	[match_presence_out] [smallint] NOT NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[private_storage](
    /* be aware of 900 bytes index length limit. nvarchar uses two bytes per char */
	[server] [nvarchar](150) NOT NULL,
	[username] [nvarchar](150) NOT NULL, -- 250 in mysql
	[namespace] [nvarchar](150) NOT NULL,
	[data] [nvarchar](max) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_private_storage] PRIMARY KEY CLUSTERED
(
	[server] ASC,
	[username] ASC,
	[namespace] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[roster_version]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[roster_version](
    [server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[version] [nvarchar](max) NOT NULL,
 CONSTRAINT [PK_roster_version_server_user] PRIMARY KEY CLUSTERED
(
    [server] ASC,
	[username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[rostergroups]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[rostergroups](
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[jid] [nvarchar](250) NOT NULL,
	[grp] [nvarchar](250) NOT NULL,
 CONSTRAINT [PK_rostergroups] PRIMARY KEY CLUSTERED
(
    [server] ASC,
	[username] ASC,
	[jid] ASC,
	[grp] ASC
))
GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[rosterusers]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[rosterusers](
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[jid] [nvarchar](250) NOT NULL,
	[nick] [nvarchar](max) NOT NULL,
	[subscription] [char](1) NOT NULL,
	[ask] [char](1) NOT NULL,
	[askmessage] [nvarchar](max) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_rosterusers] PRIMARY KEY CLUSTERED
(
    [server] ASC,
	[username] ASC,
	[jid] ASC
))
GO

SET ANSI_PADDING OFF
GO

/****** Object:  Table [dbo].[users]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[users](
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[password] [nvarchar](max) NOT NULL,
	[pass_details] [nvarchar](max) NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_users_username] PRIMARY KEY CLUSTERED
(
	[username] ASC,
        [server] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[vcard]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[vcard](
	[username] [nvarchar](150) NOT NULL,
	[server] [nvarchar](150) NOT NULL,
	[vcard] [nvarchar](max) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_vcard_username] PRIMARY KEY CLUSTERED
(
	[username] ASC,
	[server] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[vcard_search]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[vcard_search](
	[username] [nvarchar](150) NOT NULL,
	[lusername] [nvarchar](100) NOT NULL,
	[server] [nvarchar](150) NOT NULL,
	[fn] [nvarchar](max) NOT NULL,
	[lfn] [nvarchar](250) NOT NULL,
	[family] [nvarchar](max) NOT NULL,
	[lfamily] [nvarchar](250) NOT NULL,
	[given] [nvarchar](max) NOT NULL,
	[lgiven] [nvarchar](250) NOT NULL,
	[middle] [nvarchar](max) NOT NULL,
	[lmiddle] [nvarchar](250) NOT NULL,
	[nickname] [nvarchar](max) NOT NULL,
	[lnickname] [nvarchar](250) NOT NULL,
	[bday] [nvarchar](max) NOT NULL,
	[lbday] [nvarchar](250) NOT NULL,
	[ctry] [nvarchar](max) NOT NULL,
	[lctry] [nvarchar](250) NOT NULL,
	[locality] [nvarchar](max) NOT NULL,
	[llocality] [nvarchar](250) NOT NULL,
	[email] [nvarchar](max) NOT NULL,
	[lemail] [nvarchar](250) NOT NULL,
	[orgname] [nvarchar](max) NOT NULL,
	[lorgname] [nvarchar](250) NOT NULL,
	[orgunit] [nvarchar](max) NOT NULL,
	[lorgunit] [nvarchar](250) NOT NULL,
 CONSTRAINT [PK_vcard_search_lusername] PRIMARY KEY CLUSTERED
(
	[lusername] ASC,
	[server] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

CREATE TABLE [dbo].[muc_light_rooms](
    [id] [bigint] IDENTITY(1,1) NOT NULL UNIQUE,

    [luser] [NVARCHAR](200)  NOT NULL, -- 250 in mysql
    [lserver] NVARCHAR(250)  NOT NULL,
    [version] NVARCHAR(20)   NOT NULL,
    CONSTRAINT [PK_muc_light_rooms] PRIMARY KEY CLUSTERED(
        [lserver] ASC,
        [luser] ASC
    ))
GO

CREATE TABLE [dbo].[muc_light_occupants](
    [room_id] [bigint]        NOT NULL,
    [luser]   [NVARCHAR](200)  NOT NULL, -- 250 in mysql
    [lserver] [NVARCHAR](200)  NOT NULL, -- 250 in mysql
    [aff] TINYINT             NOT NULL,
    CONSTRAINT [PK_muc_light_occupants] PRIMARY KEY (
        [room_id] ASC,
        [lserver] ASC,
        [luser] ASC
    ))
GO

ALTER TABLE [dbo].[muc_light_occupants]
    ADD CONSTRAINT FK_occupants_muc_light_rooms
        FOREIGN KEY (room_id)
        REFERENCES [dbo].[muc_light_rooms](id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
GO

CREATE INDEX i_muc_light_occupants_id ON muc_light_occupants(room_id)
GO
CREATE INDEX i_muc_light_occupants_us ON muc_light_occupants(lserver, luser)
GO

CREATE TABLE [dbo].[muc_light_config](
    room_id bigint          NOT NULL,
    opt NVARCHAR(100)        NOT NULL,
    val NVARCHAR(250)        NOT NULL,
    CONSTRAINT [PK_muc_light_config] PRIMARY KEY CLUSTERED(
        [room_id] ASC,
        [opt] ASC
    ))
GO

ALTER TABLE [dbo].[muc_light_config]
    ADD CONSTRAINT FK_config_muc_light_rooms
        FOREIGN KEY (room_id)
        REFERENCES [dbo].[muc_light_rooms](id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
GO

CREATE TABLE dbo.muc_light_blocking(
    luser NVARCHAR(250)      NOT NULL,
    lserver NVARCHAR(250)    NOT NULL,
    what TINYINT            NOT NULL,
    who NVARCHAR(500)        NOT NULL
)
GO

CREATE INDEX i_muc_light_blocking_su ON muc_light_blocking(lserver, luser);
GO

-- luser, lserver and remote_bare_jid have 250 characters in MySQL
-- but here we are limited by index size (900 bytes)
CREATE TABLE dbo.inbox(
    luser NVARCHAR(150) NOT NULL,
    lserver NVARCHAR(150) NOT NULL,
    remote_bare_jid NVARCHAR(150) NOT NULL,
    msg_id NVARCHAR(250) NOT NULL,
    box NVARCHAR(64) DEFAULT 'inbox',
    content VARBINARY(max) NOT NULL,
    timestamp BIGINT NOT NULL,
    muted_until BIGINT DEFAULT 0,
    unread_count INT NOT NULL,
    CONSTRAINT PK_inbox PRIMARY KEY CLUSTERED(
        lserver ASC,
        luser ASC,
        remote_bare_jid ASC
    )
)
GO

CREATE INDEX i_inbox_su_ts ON inbox(lserver, luser, timestamp);
GO

CREATE INDEX i_inbox_us_box ON inbox(lserver, luser, box);
GO

CREATE INDEX i_inbox_box ON inbox(box);
GO

CREATE TABLE dbo.pubsub_nodes (
    nidx BIGINT           IDENTITY(1,1) PRIMARY KEY,
    p_key NVARCHAR(150)   NOT NULL,
    name NVARCHAR(150)    NOT NULL,
    type NVARCHAR(250)    NOT NULL,
    owners NVARCHAR(max)  NOT NULL,
    options NVARCHAR(max) NOT NULL
)
GO

CREATE UNIQUE INDEX i_pubsub_nodes_key_name ON pubsub_nodes(p_key, name);
GO

CREATE TABLE dbo.pubsub_node_collections (
    name NVARCHAR(150) NOT NULL,
    parent_name NVARCHAR(150) NOT NULL,
    CONSTRAINT PK_pubsub_node_collections PRIMARY KEY CLUSTERED(
        name ASC,
        parent_name ASC
    )
)
GO

CREATE TABLE dbo.pubsub_affiliations (
    nidx BIGINT NOT NULL,
    luser NVARCHAR(150) NOT NULL,
    lserver NVARCHAR(150) NOT NULL,
    aff TINYINT NOT NULL,
    CONSTRAINT PK_pubsub_affiliations PRIMARY KEY CLUSTERED(
        luser ASC,
        lserver ASC,
        nidx ASC
    )
)
GO

CREATE INDEX i_pubsub_affiliations_nidx ON pubsub_affiliations(nidx);
GO

CREATE TABLE dbo.pubsub_items (
    nidx BIGINT                      NOT NULL,
    itemid NVARCHAR(250)             NOT NULL,
    created_luser NVARCHAR(150)      NOT NULL,
    created_lserver NVARCHAR(150)    NOT NULL,
    created_at BIGINT                NOT NULL,
    modified_luser NVARCHAR(150)     NOT NULL,
    modified_lserver NVARCHAR(150)   NOT NULL,
    modified_lresource NVARCHAR(150) NOT NULL,
    modified_at BIGINT               NOT NULL,
    publisher NVARCHAR(max),
    payload VARBINARY(max)           NOT NULL,
    CONSTRAINT PK_pubsub_items PRIMARY KEY CLUSTERED(
	nidx ASC,
	itemid ASC
    )
)
GO

CREATE TABLE dbo.pubsub_last_item (
    nidx BIGINT                      NOT NULL PRIMARY KEY,
    itemid NVARCHAR(250)             NOT NULL,
    created_luser NVARCHAR(250)      NOT NULL,
    created_lserver NVARCHAR(250)    NOT NULL,
    created_at BIGINT                NOT NULL,
    payload VARBINARY(max)           NOT NULL
)
GO

-- we skip luser and lserver in this one as this is little chance (even impossible?)
-- to have itemid duplication for distinct users
CREATE INDEX i_pubsub_items_lus_nidx ON pubsub_items(created_luser, created_lserver, nidx);
GO
CREATE INDEX i_pubsub_items_nidx ON pubsub_items(nidx);
GO


CREATE TABLE dbo.pubsub_subscriptions (
    nidx BIGINT NOT NULL,
    luser NVARCHAR(150) NOT NULL,
    lserver NVARCHAR(150) NOT NULL,
    lresource NVARCHAR(150) NOT NULL,
    type TINYINT NOT NULL,
    sub_id NVARCHAR(125) NOT NULL,
    options NVARCHAR(max) NOT NULL
)
GO

CREATE INDEX i_pubsub_subscriptions_lus_nidx ON pubsub_subscriptions(luser, lserver, nidx);
GO
CREATE INDEX i_pubsub_subscriptions_nidx ON pubsub_subscriptions(nidx);
GO

CREATE TABLE event_pusher_push_subscription (
     owner_jid NVARCHAR(250),
     node NVARCHAR(250),
     pubsub_jid NVARCHAR(150),
     form NVARCHAR(max) NOT NULL,
     created_at BIGINT NOT NULL,
     CONSTRAINT PK_even_pusher_push_subscription PRIMARY KEY CLUSTERED(
	owner_jid ASC,
	node ASC,
	pubsub_jid ASC
    )
 )
GO

CREATE INDEX i_event_pusher_push_subscription ON event_pusher_push_subscription(owner_jid);
GO

SET ANSI_PADDING OFF
GO
ALTER TABLE [dbo].[offline_message] ADD  DEFAULT (NULL) FOR [expire]
GO
ALTER TABLE [dbo].[privacy_list] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[privacy_list_data] ADD  DEFAULT (NULL) FOR [id]
GO
ALTER TABLE [dbo].[private_storage] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[rosterusers] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[users] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (N'') FOR [username]
GO
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (N'') FOR [server]
GO
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[vcard_search] ADD  DEFAULT (N'') FOR [lusername]
GO
ALTER TABLE [dbo].[vcard_search] ADD  DEFAULT (N'') FOR [server]
GO

CREATE TABLE mongoose_cluster_id (
    k varchar(50) NOT NULL PRIMARY KEY,
    v text
);

CREATE TABLE muc_rooms(
    id BIGINT IDENTITY(1,1) NOT NULL UNIQUE,
    muc_host VARCHAR(250)   NOT NULL,
    room_name VARCHAR(250)       NOT NULL,
    options VARCHAR(MAX)    NOT NULL,
    PRIMARY KEY (muc_host, room_name)
);

CREATE TABLE muc_room_aff(
    room_id BIGINT          NOT NULL REFERENCES muc_rooms(id),
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    resource VARCHAR(250)   NOT NULL,
    aff SMALLINT            NOT NULL
);

CREATE INDEX i_muc_room_aff_id ON muc_room_aff (room_id);

CREATE TABLE muc_registered(
    muc_host VARCHAR(250)   NOT NULL,
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    nick VARCHAR(250)       NOT NULL,
    PRIMARY KEY (muc_host, luser, lserver)
);

-- from_jid, to_jid and thread have 250 characters in MySQL
-- but here we are limited by index size (900 bytes)
CREATE TABLE smart_markers (
    lserver NVARCHAR(100) NOT NULL,
    luser NVARCHAR(150) NOT NULL,
    to_jid NVARCHAR(250) NOT NULL,
    thread NVARCHAR(80) NOT NULL,
    -- chat marker types:
    -- 'R' - received
    -- 'D' - displayed
    -- 'A' - acknowledged
    type NVARCHAR(1) NOT NULL,
    msg_id NVARCHAR(250) NOT NULL,
    timestamp BIGINT NOT NULL,
    CONSTRAINT pk_smart_markers PRIMARY KEY CLUSTERED(
            lserver ASC,
            luser ASC,
            to_jid ASC,
            thread ASC,
            type ASC
        )
);

CREATE INDEX i_smart_markers ON smart_markers(to_jid, thread);

-- jid, thread and room have 250 characters in MySQL
-- but here we are limited by index size (900 bytes)
CREATE TABLE offline_markers (
    jid NVARCHAR(150) NOT NULL,
    thread NVARCHAR(145) NOT NULL,
    room NVARCHAR(150) NOT NULL,
    timestamp BIGINT NOT NULL,
    CONSTRAINT pk_offline_markers PRIMARY KEY CLUSTERED(
                jid ASC,
                thread ASC,
                room ASC
        )
);

CREATE INDEX i_offline_markers ON offline_markers(jid);

CREATE TABLE domain_admins(
     domain VARCHAR(250) NOT NULL PRIMARY KEY,
     pass_details NVARCHAR(max) NOT NULL
);

-- Mapping from domain hostname to host_type.
-- Column id is used for ordering only.
CREATE TABLE domain_settings (
    id BIGINT IDENTITY(1,1) PRIMARY KEY,
    domain VARCHAR(250) NOT NULL,
    host_type VARCHAR(250) NOT NULL,
    status SMALLINT NOT NULL DEFAULT 1
);

-- A new record is inserted into domain_events, each time
-- domain_settings table is updated.
-- Column id is used for ordering and not related to domain_settings.id.
CREATE TABLE domain_events (
    id BIGINT IDENTITY(1,1) PRIMARY KEY,
    domain VARCHAR(250) NOT NULL
);
CREATE INDEX i_domain_events_domain ON domain_events(domain);

CREATE TABLE discovery_nodes (
    cluster_name varchar(250) NOT NULL,
    node_name varchar(250) NOT NULL,
    node_num INT NOT NULL,
    address varchar(250) NOT NULL DEFAULT '', -- empty means we should ask DNS
    updated_timestamp BIGINT NOT NULL, -- in seconds
    PRIMARY KEY (node_name)
);
CREATE UNIQUE INDEX i_discovery_nodes_node_num ON discovery_nodes(cluster_name, node_num);

CREATE TABLE caps (
    node varchar(250) NOT NULL,
    sub_node varchar(250) NOT NULL,
    features text NOT NULL,
    PRIMARY KEY (node, sub_node)
);
