# Provider

## Pools
Tools for promoting qualifications among target audience.

Goal - primary user are able to search various qualification to be added to trajectory by
2 approaches. first via search bar (let's call it active), second via promoted by provider by ad-hoc tools (provider may want to promote a qualification for target audience).

1. **Tag**
Definition: **tags allow to promote the given qualification for target audience employing data taken from primary user (e.g. his qualififcation, additional skills, etc)**.
Tages are created not linked to any data at edgenode. Provider creates any tags that he wants.
 Available handles:
   - `PUT /provider/pools/tags` - create new tags
   - `DELETE /provider/pools/tags` - purge tags
   - `DELETE /provider/pools/tags` - purge tags
   - `PATCH /provider/pools/tags` - patch tags
