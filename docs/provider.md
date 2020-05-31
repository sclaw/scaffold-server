# Provider

## Qualification

1. Dependency
  Cluster: each cluster may be titled. by default, if no title are passed, title is
  qualification_title_n, where n is serial number


## Pools
Tools for promoting qualifications among target audience.

Goal - primary user are able to search various qualification to be added to trajectory by
2 approaches. first via search bar (let's call it active), second via promoted by provider by ad-hoc tools (provider may want to promote a qualification for target audience).

Table at which promoted qualification, matched primary user are strored. Fiiled after new tags was added.
```
edgenode.provider_branch_promoted_qualification_user (
  id: int8, serial,
  qualification_fk: int8 not null refer to edgenode.provider_branch_qualification(id),
  user_fk: int8 not null refer to auth.user(id),
  trajectory_status: text not null (refers to TrajectoryStatus),
  created: timestamp not null default now,
  processed: timestamp null,
  promoted_type: text not null
)
```

[TrajectoryStatus](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)

```
data TrajectoryStatus = TrajectoryStatusNew | TrajectoryStatusConfirmed | TrajectoryStatusRejected
```

[PromotedType](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)

```
data PromotedType = PromotedTypeTags
```

user data taken from profile. We use the same technique that at searching: get necessary data, tokenize it by ts_vector.
To search appropriate token we employ trigram at our service.
```
edgenode.user_profile_token (
  user_fk: int8 not null refers to auth.user(id),
  token: text not null),

edgenode.user_qualification_token (
  user_fk: int8 not null refers to auth.user(id),
  provider_branch_qualification_fk: int8 not null refers to edgenode.provider_branch_qualification(id),
  token text not null)

```
```
select word_similarity('сам', v)
from unnest(tsvector_to_array(to_tsvector(text))) as x(v)
```

1. **Tag**
Definition: **tags allow to promote the given qualification for target audience employing data taken from primary user (e.g. his qualififcation, additional skills, etc)**.
Tages are created not linked to any data at edgenode. Provider creates any tags that he wants.

 Handles:
   - `PUT /provider/pool/tags` - create new tags
     Request: [message TagsBuilder](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)
     Response: `uint64`
   - `DELETE /provider/pool/tags/{tags_id}` - purge tags
     Request: empty
     Response: `unit`
     Error: tags not found
   - `GET /provider/pool/tags/list` - list all tags
     Request: empty
     Response: `TagsList`
   - `PATCH /provider/pool/tags/{tags_id}` - patch tags
     Request: [message TagsPatch](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)
     Response: `unit`
     Error: tags not found
   - `POST /provider/pool/tags/builder/affected-audience` - number of primary users affected by tags Querty are send by putting tag.
     Request: [message AffectedAudience](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)
     Response: `uint32`
    - `GET /provider/pool/tags/builder/{qualification_id}/clusters` - qualification's cluster
    Request: empty
    Response: [message QualificationCluster](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)
    - `POST /provider/pool/tags/{tags_id}/publish` - publish  tags

 Sql tables:
  ```
   edgenode.provider_pool_tags (
      id: int8, serial,
      ttile: text not null,
      qualification_fk: int8 not null refer to edgenode.provider_branch_qualification(id)
  ),
  edgenode.provider_pool_tags_value (
      value: text not null,
      tags_fk: int8 not null refer to edgenode.pool_tags(id),
      unique: (value, tags_fk)
  ),
  edgenode.provider_pool_tags_cluster (
      cluster_fk: int8 not null refer to edgenode.provider_branch_qualification_dependency_cluster(id),
      tags_fk: int8 not null refer to edgenode.pool_tags(id),
      unique: (cluster_fk, tags_fk)
  )
  ```

Validation: