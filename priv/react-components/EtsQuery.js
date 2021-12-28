import React from 'react';
import EtsQueryPanel from './EtsQueryPanel';
const EtsQuery = (props) => {
    let jsx = '';
    //console.log('EtsQuery props.viewQuery = '+props.viewQuery);
    if( props.viewQuery ) {
        jsx = <EtsQueryPanel
            queryTables={props.queryTables}
            lookupQuery={props.lookupQuery}
            matchQuery={props.matchQuery}
            matchObjectQuery={props.matchObjectQuery}
        />;
    }
    return jsx
};
export default EtsQuery;