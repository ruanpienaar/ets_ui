import React from 'react';
import EtsTableViewRows from './EtsTableViewRows';
import EtsTableViewPagesize from './EtsTableViewPagesize';
import EtsTableViewNextRows from './EtsTableViewNextRows';
const EtsTableView = (props) => (
    <div>
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