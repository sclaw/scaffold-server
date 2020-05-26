# Provider

## Pools
Tools for promoting qualifications among target audience.

Goal - primary user are able to search various qualification to be added to trajectory by
2 approaches. first via search bar (let's call it active), second via promoted by provider by ad-hoc tools (provider may want to promote a qualification for target audience).

1. **Tag**
Definition: **tags allow to promote the given qualification for target audience employing data taken from primary user (e.g. his qualififcation, additional skills, etc)**.
Tages are created not linked to any data at edgenode. Provider creates any tags that he wants.

 Handles:
   - `PUT /provider/pools/tags` - create new tags
     Request: [message TagsBuilder](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)
     Response: `int64`
   - `DELETE /provider/pools/tags/{tags_id}` - purge tags
     Request: empty
     Response: `unit`
     Error: tags not found
   - `GET /provider/pools/tags/list` - list all tags
     Request: empty
     Response: `TagsList`
   - `PATCH /provider/pools/tags/{tags_id}` - patch tags
     Request: [message TagsPatch](https://gitlab.com/edgenode2/proto/-/blob/master/EdgeNode/Transport/Pool/Tags.proto)
     Response: `unit`
     Error: tags not found

 Sql tables:
  ``edgenode.pool_tags (
      id: int8, serial,
      ttile: text not null
  )``

Validation:
