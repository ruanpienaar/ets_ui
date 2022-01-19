import React from 'react';
import EtsQuery from './EtsQuery';
import EtsTableViewRows from './EtsTableViewRows';
import EtsTableViewPagesize from './EtsTableViewPagesize';
import EtsTableViewNextRows from './EtsTableViewNextRows';
const EtsTableView = (props) => (
    <div>
        <EtsQuery
            viewQuery={props.viewQuery}
            queryTables={props.queryTables}
            lookupQuery={props.lookupQuery}
            matchQuery={props.matchQuery}
            matchObjectQuery={props.matchObjectQuery}
        />
        <hr />
        <EtsTableViewPagesize
            pagesize={props.pagesize}
            pagesize_options={props.pagesize_options}
            changePagesize={props.changePagesize}
        />
        <EtsTableViewNextRows
            nextEntries={props.nextEntries}
            continuation={props.continuation}
        />
        <EtsTableViewRows rows={props.rows} />
    </div>
);
export default EtsTableView;