import React from "react";
import "./Container.css";
import classNames from "classnames";
import { ClassValue } from "classnames/types";

export interface Props {
  className?: ClassValue[];
}

export class Container extends React.PureComponent<Props> {
  render() {
    return (
      <div className={classNames("container", this.props.className)}>
        {this.props.children}
      </div>
    );
  }
}
