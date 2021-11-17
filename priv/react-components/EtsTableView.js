import React from 'react';
import EtsTableViewRows from './EtsTableViewRows';

const EtsTableView = (props) => (
    <div>
        <hr />
        <button>hide results</button>
        <select id="pagesize" className="btn btn-primary">
            <option value="3">3</option>
            <option value="20">20</option>
            <option value="50">50</option>
            <option value="100">100</option>
        </select>
        <EtsTableViewRows rows={props.rows} />
    </div>
);

export default EtsTableView;